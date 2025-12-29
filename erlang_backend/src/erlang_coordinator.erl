
-module(erlang_coordinator).
-behavior(gen_server).
-export([start_link/0, init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2]).

-define(WORKER_NODES, ['worker1@10.2.1.40', 'worker2@10.2.1.45', 'worker3@10.2.1.46']).
-define(JAVA_NODE, 'java_backend_node@10.2.1.39').
-include("shared.hrl").
%% Avvio del server
start_link() ->
  gen_server:start_link({local, coordinator_service}, ?MODULE, [], []).

%% Inizializzazione
init([]) ->
  io:format(">> Coordinatore avviato~n"),
  WorkerList = lists:filtermap(fun(Node) ->
    case net_adm:ping(Node) of
      pong ->
        Pid = spawn(Node, erlang_actor, start, []),
        io:format(">> Worker attivo su ~p: ~p~n", [Node, Pid]),
        {true, {Node, Pid}};
      pang ->
        io:format(">> Errore: Nodo ~p non raggiungibile~n", [Node]),
        false
    end
  end, ?WORKER_NODES),

  %% Stato iniziale con workers e mappa sessioni vuota
  {ok, #{
    workers => WorkerList,
    sessioni => #{}  %% EventId => #{received => 0, best => undefined}
  }}.

%% RICEZIONE DA JAVA: Nuovo Vincolo
handle_info({nuovo_vincolo, _, _, _, _, _, _, _, _, Posizione} = Msg, State) ->
  io:format(">> Ricevuto vincolo per zona (stringa): ~p~n", [Posizione]),

  %% Determiniamo il nodo di destinazione confrontando stringhe
  TargetNode = case Posizione of
                 "NORD"   -> 'worker1@127.0.0.1';
                 "CENTRO" -> 'worker2@127.0.0.1';
                 "SUD"    -> 'worker3@127.0.0.1';
                 _        ->
                   %% Debug: stampa il valore numerico se non combacia
                   io:format(">> Valore ricevuto non riconosciuto: ~p~n", [Posizione]),
                   undefined
               end,

  case TargetNode of
    undefined ->
      {noreply, State};
    Node ->
      io:format(">> Inoltro al worker di zona: ~p~n", [Node]),
      {vincolo_service, Node} ! Msg,
      {noreply, State}
  end;

%% RICEZIONE DA JAVA: Calcolo Ottimo
handle_info({calcola_ottimo_globale, EventId}, State) ->
  io:format(">> Richiesta ottimo globale per evento ~p a tutti i worker...~n", [EventId]),
  Workers = maps:get(workers, State),
  %% Inviamo la richiesta a tutti i worker registrati
  lists:foreach(fun({_Node, Pid}) ->
    Pid ! {richiedi_parziale, self(), EventId}
    end, Workers),
  %% Inizializziamo la sessione nella mappa
  NuovaSessione = #{received => 0, best => undefined},
  SessioniAggiornate = maps:put(EventId, NuovaSessione, maps:get(sessioni, State)),
  {noreply, State#{sessioni => SessioniAggiornate}};




%% Ricezione della risposta dal Worker
handle_info({risposta_parziale, EventId, Sol}, State) ->
  Sessioni = maps:get(sessioni, State),

  case maps:find(EventId, Sessioni) of
    {ok, SessInfo} ->
      NuovoReceived = maps:get(received, SessInfo) + 1,
      NuovoBest = confronta_ottimo(maps:get(best, SessInfo), Sol),
      NumWorkers = length(maps:get(workers, State)),

      if
        NuovoReceived >= NumWorkers ->
          %% Tutti i worker hanno risposto: chiudiamo e inviamo a Java
          io:format(">> Evento ~p concluso. Ottimo: ~p~n", [EventId, NuovoBest]),
          {java_mailbox, ?JAVA_NODE} ! {risultato_finale, EventId, NuovoBest},
          %% Pulizia: rimuoviamo la sessione conclusa
          {noreply, State#{sessioni => maps:remove(EventId, Sessioni)}};
        true ->
          %% Aggiorniamo la sessione in attesa degli altri worker
          SessAggiornata = SessInfo#{received => NuovoReceived, best => NuovoBest},
          {noreply, State#{sessioni => maps:put(EventId, SessAggiornata, Sessioni)}}
      end;
    error ->
      %% Messaggio per una sessione inesistente o già chiusa
      {noreply, State}
  end.

%% Callback obbligatorie
handle_call(_Req, _From, State) -> {reply, ok, State}.
handle_cast(_Msg, State) -> {noreply, State}.
terminate(_Reason, _State) -> ok.

%% Logica di confronto (esempio semplificato)
%% --- Funzione per determinare la soluzione migliore ---

%% Caso 1: Il worker non ha trovato nulla (restituisce 'empty')
%% Manteniamo il miglior risultato attuale (che sia undefined o un record)
confronta_ottimo(CurrentBest, empty) ->
  CurrentBest;

%% Caso 2: È la prima risposta valida che riceviamo (CurrentBest è ancora undefined)
confronta_ottimo(undefined, NuovaSol) ->
  NuovaSol;

%% Caso 3: Abbiamo già un record e ne riceviamo uno nuovo dal worker
%% Estraiamo lo 'score' da entrambi i record #best_solution e confrontiamo
confronta_ottimo(CurrentBest, NuovaSol) ->
  ScoreCorrente = CurrentBest#best_solution.score,
  NuovoScore = NuovaSol#best_solution.score,

  if
    NuovoScore > ScoreCorrente ->
      NuovaSol; %% Il nuovo è migliore
    true ->
      CurrentBest %% Il vecchio resta il migliore
  end.