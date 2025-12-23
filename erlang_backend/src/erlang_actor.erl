-module(erlang_actor).
-export([start/0, loop/1]).
-record(state, {total_users = 0}). %% Stato interno dell'attore

-include("shared.hrl").

start() ->
  db_manager:init(),
  db_manager:add_locale(1, "Golden Pub", "Pub", 15.0, {18, 24}),
  db_manager:add_locale(2, "Pizza Express", "Ristorante", 25.0, {19, 23}),

  io:format(">> Attore Remoto pronto su ~p.~n", [node()]),

  %% IMPORTANTE: Registriamo il nome qui internamente
  register(vincolo_service, self()),

  %% Entriamo direttamente nel loop, non facciamo un altro spawn!
  loop(#state{total_users = 0}).

loop(State) ->
  receive
    {nuovo_vincolo, Id, IdEvento, Email, OraInizio, OraFine, BudMin, BudMax, TipoPreferito, _Pos} ->

      io:format("--- ELABORAZIONE VINCOLO ID: ~p ---~n", [Id]),
      io:format("Evento: ~p | Utente: ~p | Orario: ~p - ~p | Budget: ~p - ~p | TipoPref: ~p~n",
        [IdEvento, Email, OraInizio, OraFine, BudMin, BudMax, TipoPreferito]),

      %% 1. Incrementiamo il contatore globale utenti
      NewTotalUsers = State#state.total_users + 1,

      %% 2. Recuperiamo tutti i locali gestiti da questo nodo
      Locali = db_manager:get_locali(),

      %% 3. Ciclo su ogni locale per calcolo incrementale
      lists:foreach(fun(L) -> processa_locale(L, {OraInizio, OraFine, BudMin, BudMax, TipoPreferito}, NewTotalUsers) end, Locali),

      loop(State#state{total_users = NewTotalUsers});

    {calcola_ottimo_globale, Id,Pid_coordinatore} ->
      1+1;
    {richiedi_parziale, Pid_coordinatore} ->
      io:format(">> Worker: Ricevuta richiesta da ~p~n", [Pid_coordinatore]),
      %% 1. Leggiamo l'ottimo da Mnesia
      {atomic, Res} = mnesia:transaction(fun() ->
                                        mnesia:read(best_solution, node())
                                                                         end),

      %% 2. Prepariamo la risposta
      Risposta = case Res of
                   [Best] -> Best; %% Invia il record completo
                   []     -> empty
                 end,

      %% 3. INVIO AL COORDINATORE (Questo è il pezzo mancante!)
      Pid_coordinatore ! {risposta_parziale, Risposta},

      loop(State);



    Other ->
      io:format("Messaggio sconosciuto: ~p~n", [Other]),
      loop(State)
  end.

processa_locale(Locale, {U_Start, U_End, BMin, BMax, TipoPref}, TotUsers) ->
  %% Estrazione dati locale
  {locale, L_Id, _Nome, L_Tipo, L_Prezzo, L_Apertura, L_Chiusura} = Locale,

  %% A. Calcolo Pqualità specifico per questo utente
  QualitaUtente = utils:calcola_qualita_utente(L_Tipo, L_Prezzo, TipoPref, BMin, BMax),
  io:format("Locale ~p -> QualitaUtente calcolata: ~.2f~n", [L_Id, QualitaUtente]),

  %% B. Calcolo Sovrapposizione Oraria
  SlotSovrapposti = utils:calcola_slot_sovrapposti(U_Start, U_End, L_Apertura, L_Chiusura),
  io:format("Locale ~p -> Slot Sovrapposti: ~p~n", [L_Id, SlotSovrapposti]),

  %% C. Aggiornamento DB (Mnesia) e recupero nuovi aggregati
  {atomic, {NewAvgQual, MaxSlotCount}} = db_manager:update_stats(L_Id, QualitaUtente, SlotSovrapposti, TotUsers),

  %% D. Calcolo Itot aggiornato
  Itot = utils:calcola_itot(NewAvgQual, MaxSlotCount, TotUsers, L_Id),
  io:format("Locale ~p -> Itot calcolato: ~.4f~n", [L_Id, Itot]),
  
  io:format("Locale ~p -> QualitaUtente: ~.2f | NewAvg: ~.2f | MaxPartecipanti: ~p | Itot: ~.4f~n",
    [L_Id, QualitaUtente, NewAvgQual, MaxSlotCount, Itot]),

  %% E. Verifica se è la nuova soluzione ottimale
  check_and_update_best(L_Id, Itot).

check_and_update_best(LocaleId, Itot) ->
  F = fun() ->
    CurrentBest = mnesia:read(best_solution, node()),
    Update = case CurrentBest of
               [] -> true;
               [#best_solution{score=OldScore}] when Itot > OldScore -> true;
               _ -> false
             end,

    if Update ->
      io:format(">>> NUOVO RECORD LOCALE! Locale ~p con Score ~.4f~n", [LocaleId, Itot]),
      mnesia:write(#best_solution{id_nodo=node(), id_locale=LocaleId, ora_inizio=0, score=Itot});
      true -> ok
    end
      end,
  mnesia:transaction(F).