-module(erlang_coordinator).
-behavior(gen_server).
-export([start_link/0, init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2]).

%% Costanti
-define(WORKER_NODES, ['worker1@127.0.0.1', 'worker2@127.0.0.1', 'worker3@127.0.0.1']).
-define(JAVA_NODE, 'java_backend_node@127.0.0.1').
-include("shared.hrl").

%% Avvia il processo gen_server
start_link() ->
  gen_server:start_link({local, coordinator_service}, ?MODULE, [], []).

%% Inizializzazione del coordinatore
init([]) ->
  io:format(">> [COORDINATOR] Avviato. Tento connessione ai worker...~n"),

  %% Tenta di connettersi attivamente a tutti i nodi noti
  lists:foreach(fun(Node) ->
    io:format(">> Ping verso ~p... ", [Node]),
    io:format("~p~n", [net_adm:ping(Node)])
                end, ?WORKER_NODES),

  %% Monitoraggio nodi
  net_kernel:monitor_nodes(true),

  {ok, #{
    worker_nodes => ?WORKER_NODES,
    sessioni => #{}
  }}.

%% --- RICEZIONE DA JAVA: Nuovo Vincolo ---
handle_info({nuovo_vincolo, _, _, _, _, _, _, _, _, Posizione} = Msg, State) ->
  io:format(">> Ricevuto vincolo per zona: ~p~n", [Posizione]),

  %% Determina il nodo target in base alla posizione
  TargetNode = case Posizione of
                 "NORD"   -> 'worker1@127.0.0.1';
                 "CENTRO" -> 'worker2@127.0.0.1';
                 "SUD"    -> 'worker3@127.0.0.1';
                 _ -> undefined
               end,

  %% Inoltra il vincolo al nodo target se raggiungibile
  case TargetNode of
    undefined -> {noreply, State};
    Node ->
     case net_adm:ping(Node) of
        pong ->
          io:format(">> Inoltro a {vincolo_service, ~p}~n", [Node]),
          {vincolo_service, Node} ! Msg;
        pang ->
          io:format("!!! ERRORE: Nodo ~p irraggiungibile (giù o crashato). Vincolo perso.~n", [Node])
      end,
      {noreply, State}
  end;

%% --- RICEZIONE DA JAVA: Calcolo Ottimo Globale ---
handle_info({calcola_ottimo_globale, EventId}, State) ->
  io:format(">> Richiesta ottimo globale per evento ~p~n", [EventId]),
  AllNodes = maps:get(worker_nodes, State),

  %% Filtriamo solo i nodi attivi
  ActiveNodes = lists:filter(fun(N) -> lists:member(N, nodes()) end, AllNodes),

  %% Inviamo la richiesta solo ai nodi vivi
  lists:foreach(fun(Node) ->
    {vincolo_service, Node} ! {richiedi_parziale, self(), EventId}
                end, ActiveNodes),

  NumExpected = length(ActiveNodes),
  io:format(">> Attendo ~p risposte (su ~p nodi totali configurati)~n", [NumExpected, length(AllNodes)]),

  if NumExpected == 0 ->
    %% Caso limite: nessun worker vivo
    {java_mailbox, ?JAVA_NODE} ! {risultato_finale, EventId, empty},
    {noreply, State};
    true ->
      NuovaSessione = #{received => 0, best => undefined, expected => NumExpected},
      SessioniAggiornate = maps:put(EventId, NuovaSessione, maps:get(sessioni, State)),
      {noreply, State#{sessioni => SessioniAggiornate}}
  end;

%% --- RICEZIONE RISPOSTA PARZIALE ---
handle_info({risposta_parziale, EventId, Sol}, State) ->
  Sessioni = maps:get(sessioni, State),
  case maps:find(EventId, Sessioni) of
    {ok, SessInfo} ->
      NuovoReceived = maps:get(received, SessInfo) + 1,
      NuovoBest = confronta_ottimo(maps:get(best, SessInfo), Sol),
      NumExpected = maps:get(expected, SessInfo),

      if
        NuovoReceived >= NumExpected ->
          io:format(">> Evento ~p concluso. Ottimo: ~p~n", [EventId, NuovoBest]),
          {java_mailbox, ?JAVA_NODE} ! {risultato_finale, EventId, NuovoBest},
          {noreply, State#{sessioni => maps:remove(EventId, Sessioni)}};
        true ->
          SessAggiornata = SessInfo#{received => NuovoReceived, best => NuovoBest},
          {noreply, State#{sessioni => maps:put(EventId, SessAggiornata, Sessioni)}}
      end;
    error ->
      {noreply, State}
  end;

%% Monitoraggio Nodi
handle_info({nodedown, Node}, State) ->
  io:format("!!! ALLARME: Nodo ~p caduto.~n", [Node]),
  {noreply, State};

handle_info({nodeup, Node}, State) ->
  io:format(">> INFO: Nodo ~p connesso.~n", [Node]),
  {noreply, State}. %% <--- QUESTO PUNTO È FONDAMENTALE (Fine handle_info)

%% Callback obbligatorie
handle_call(_Req, _From, State) -> {reply, ok, State}.
handle_cast(_Msg, State) -> {noreply, State}.
terminate(_Reason, _State) -> ok.

%% Logica di confronto per trovare l'ottimo tra le soluzioni che arrivano
%% dalle varie istanze worker
confronta_ottimo(CurrentBest, empty) -> CurrentBest;
confronta_ottimo(undefined, NuovaSol) -> NuovaSol;
confronta_ottimo(CurrentBest, NuovaSol) ->
  ScoreCorrente = CurrentBest#best_solution.score,
  NuovoScore = NuovaSol#best_solution.score,
  if NuovoScore > ScoreCorrente -> NuovaSol; true -> CurrentBest end.