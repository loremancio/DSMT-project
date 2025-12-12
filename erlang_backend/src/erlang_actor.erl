-module(erlang_actor).
-export([start/0, loop/0]).

start() ->
  Pid = spawn_link(?MODULE, loop, []),

  register(vincolo_service, Pid),

  io:format(">> Attore Vincoli avviato correttamente (PID: ~p)~n", [Pid]),

  {ok, Pid}.

loop() ->
  receive
    {nuovo_vincolo, Id, IdEvento, Email, OraInizio, OraFine, BudMin, BudMax, Luogo, Posizione} ->
      io:format("~n--- NUOVO VINCOLO RICEVUTO [ID: ~p] ---~n", [Id]),
      io:format("Utente: ~p~n", [Email]),
      io:format("Evento: ~p~n", [IdEvento]),
      io:format("Orario: ~p - ~p~n", [OraInizio, OraFine]),
      io:format("Budget: ~p - ~p~n", [BudMin, BudMax]),
      io:format("Luogo:  ~p ~p~n", [Luogo, Posizione]),
      loop();

    Other ->
      io:format("Messaggio sconosciuto: ~p~n", [Other]),
      loop()
  end.