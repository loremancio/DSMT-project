%%%-------------------------------------------------------------------
%%% @author Matteo
%%% @copyright (C) 2025, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 23. dic 2025 22:18
%%%-------------------------------------------------------------------
-module(erlang_coordinator).
-author("Matteo").
-behavior(gen_server).
-export([start_link/0, init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2]).

-define(WORKER_NODE, 'worker1@127.0.0.1').

%% Avvio del server
start_link() ->
  gen_server:start_link({local, coordinator_service}, ?MODULE, [], []).

%% Inizializzazione
init([]) ->
  io:format(">> Coordinatore avviato. Tentativo di connessione a ~p...~n", [?WORKER_NODE]),
  net_adm:ping(?WORKER_NODE),

  %% Avviamo l'attore. Poiché erlang_actor:start() ora chiama loop/1,
  %% questo processo remoto rimarrà vivo.
  Pid = spawn(?WORKER_NODE, erlang_actor, start, []),

  io:format(">> Worker remoto avviato con successo: ~p~n", [Pid]),
  {ok, #{worker_pid => Pid}}.

%% RICEZIONE DA JAVA: Nuovo Vincolo
handle_info({nuovo_vincolo, _, _, _, _, _, _, _, _, _} = Msg, State) ->
  io:format(">> Inoltro vincolo all'unico worker remoto...~n"),

  %% Inoltro diretto al worker registrato sul nodo remoto
  {vincolo_service, ?WORKER_NODE} ! Msg,

  {noreply, State};

%% RICEZIONE DA JAVA: Calcolo Ottimo
handle_info({calcola_ottimo_globale, EventId}, State) ->
  io:format(">> Richiesta ottimo per evento ~p. Interrogo il worker...~n", [EventId]),

  %% Chiediamo l'ottimo al worker
  {vincolo_service, ?WORKER_NODE} ! {richiedi_parziale, self()},
  {noreply, State};
%% Ricezione della risposta dal Worker
handle_info({risposta_parziale, Sol}, State) ->
  io:format(">> Ricevuto risultato dal worker. Lo spedisco a Java...~n"),

  %% Qui userai la tua funzione invia_a_java/2 vista prima
  %% Per ora facciamo solo un log

  %% MODIFICAREEEEEEEEEEEEEEEEEE
  io:format(">> Risultato finale: ~p~n", [Sol]),
  {java_mailbox, 'java_backend_node@127.0.0.1'} ! {risultato_finale, Sol},
  {noreply, State}.

%% Callback standard obbligatorie (lasciale vuote)
handle_call(_Req, _From, State) -> {reply, ok, State}.
handle_cast(_Msg, State) -> {noreply, State}.
terminate(_Reason, _State) -> ok.