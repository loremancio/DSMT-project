-module(erlang_coordinator).
-behavior(gen_server).
-export([start_link/0, init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2]).

-define(JAVA_NODE, 'java_backend_node@10.2.1.39').
-include("shared.hrl").

%% Avvia il processo gen_server
start_link() ->
  gen_server:start_link({local, coordinator_service}, ?MODULE, [], []).

%% Inizializzazione del coordinatore
init([]) ->
  io:format(">> [COORDINATOR] Avviato. Avvio discovery dinamico (pg)...~n"),
  pg:start(pg), %% Avvia il gestore dei gruppi

  net_kernel:monitor_nodes(true),

  {ok, #{
    sessioni => #{},
    timers => #{}
  }}.

%% --- RICEZIONE DA JAVA: Nuovo Vincolo ---
handle_info({nuovo_vincolo, _, _, _, _, _, _, _, _, Posizione} = Msg, State) ->
  io:format(">> Ricevuto vincolo per zona: ~p~n", [Posizione]),

  %% 1. TRASFORMAZIONE DINAMICA: "NORD" -> 'gruppo_nord'
  GroupNameStr = "gruppo_" ++ string:to_lower(Posizione),
  TargetGroup = try list_to_existing_atom(GroupNameStr)
                catch error:badarg -> undefined end,

  case TargetGroup of
    undefined ->
      io:format(">> [WARN] Nessun gruppo attivo per la zona: ~p~n", [Posizione]);
    Group ->
      %% 2. CHIEDIAMO A PG: Chi c'è in questo gruppo?
      Members = pg:get_members(Group),
      case Members of
        [] ->
          io:format(">> [ERR] Il gruppo ~p esiste ma è vuoto (nessun worker).~n", [Group]);
        [WorkerPid | _] ->
          io:format(">> Inoltro a ~p (Membro di ~p)~n", [WorkerPid, Group]),
          WorkerPid ! Msg
      end
  end,
  {noreply, State};

handle_info({deadline, EventId, Delay}, State) ->
  Timers = maps:get(timers, State),

  case maps:is_key(EventId, Timers) of
    true ->
      io:format(">> [IGNORATO] Timer per Evento ~p già presente.~n", [EventId]),
      {noreply, State};

    false ->
      io:format(">> [TIMER] Schedulato Evento ~p tra ~p ms~n", [EventId, Delay]),


      _TimerRef = erlang:send_after(Delay, self(), {calcola_ottimo_globale, EventId}),


      NewTimers = maps:put(EventId, active, Timers),
      {noreply, State#{timers => NewTimers}}
  end;
%% --- RICEZIONE DA JAVA: Calcolo Ottimo Globale ---
handle_info({calcola_ottimo_globale, EventId}, State) ->
  io:format(">> Richiesta ottimo globale per evento ~p~n", [EventId]),
  Timers = maps:get(timers, State),
  NewTimers = maps:remove(EventId, Timers),

  %% 3. BROADCAST GLOBALE tramite il gruppo 'tutti_i_worker'
  AllWorkers = pg:get_members('tutti_i_worker'),
  UniqueWorkers = lists:usort(AllWorkers), %% Rimuove duplicati

  NumExpected = length(UniqueWorkers),
  io:format(">> Attendo ~p risposte (da worker dinamici)~n", [NumExpected]),

  if NumExpected == 0 ->
    {java_mailbox, ?JAVA_NODE} ! {risultato_finale, EventId, empty},
    {noreply, State};
    true ->
      %% Inviamo la richiesta a tutti
      lists:foreach(fun(Pid) ->
        Pid ! {richiedi_parziale, self(), EventId}
                    end, UniqueWorkers),

      NuovaSessione = #{received => 0, best => undefined, expected => NumExpected},
      SessioniAggiornate = maps:put(EventId, NuovaSessione, maps:get(sessioni, State)),
      {noreply, State#{sessioni => SessioniAggiornate, timers => NewTimers}}
  end;


%% --- RICEZIONE RISPOSTA PARZIALE (Logica Pesata) ---
handle_info({risposta_parziale, EventId, {RecordLocale, Hits}}, State) ->
  Sessioni = maps:get(sessioni, State),
  case maps:find(EventId, Sessioni) of
    {ok, SessInfo} ->

      CurrentBestTuple = maps:get(best, SessInfo),
      NuovoBestTuple = confronta_ottimo_pesato(CurrentBestTuple, {RecordLocale, Hits}),

      NuovoReceived = maps:get(received, SessInfo) + 1,
      NumExpected = maps:get(expected, SessInfo),

      if
        NuovoReceived >= NumExpected ->
          {VincitoreRecord, _} = NuovoBestTuple,
          io:format(">> [WINNER] Evento ~p concluso. Vince: ~p~n",
            [EventId, case VincitoreRecord of undefined -> "Nessuno"; _ -> VincitoreRecord#best_solution.nome_locale end]),

          {java_mailbox, ?JAVA_NODE} ! {risultato_finale, EventId, VincitoreRecord},

          NewSessioni = maps:remove(EventId, Sessioni),
          {noreply, State#{sessioni => NewSessioni}};

        true ->
          SessAggiornata = SessInfo#{received => NuovoReceived, best => NuovoBestTuple},
          {noreply, State#{sessioni => maps:put(EventId, SessAggiornata, Sessioni)}}
      end;
    error -> {noreply, State}
  end;

%% Monitoraggio Nodi (Solo log)
handle_info({nodedown, Node}, State) ->
  io:format("!!! ALLARME: Nodo ~p caduto.~n", [Node]),
  {noreply, State};
handle_info({nodeup, Node}, State) ->
  io:format(">> INFO: Nodo ~p connesso.~n", [Node]),
  {noreply, State}.

handle_call(_Req, _From, State) -> {reply, ok, State}.
handle_cast(_Msg, State) -> {noreply, State}.
terminate(_Reason, _State) -> ok.

%% Logica di confronto pesata (Score * Hits)
confronta_ottimo_pesato(undefined, {empty, _}) -> {undefined, 0};
confronta_ottimo_pesato(undefined, {NewRec, NewHits}) -> {NewRec, NewHits};
confronta_ottimo_pesato({CurRec, CurHits}, {empty, _}) -> {CurRec, CurHits};
confronta_ottimo_pesato({undefined, _}, {NewRec, NewHits}) -> {NewRec, NewHits};
confronta_ottimo_pesato({CurRec, CurHits}, {NewRec, NewHits}) ->
  CurrentImpact = CurRec#best_solution.score * CurHits,
  NewImpact = NewRec#best_solution.score * NewHits,

  io:format("   [CONFRONTO] ~p (Impact: ~.2f) vs ~p (Impact: ~.2f)~n",
    [CurRec#best_solution.nome_locale, CurrentImpact,
      NewRec#best_solution.nome_locale, NewImpact]),

  if
    NewImpact > CurrentImpact -> {NewRec, NewHits};
    true -> {CurRec, CurHits}
  end.