-module(erlang_actor).
-export([start/0, loop/1]).
-record(state, {total_users = 0}).

-include("shared.hrl").

start() ->
  db_manager:init(),

  %% --- 1. CARICAMENTO DIFFERENZIATO PER NODO ---
  NodeName = atom_to_list(node()),

  %% Definiamo i locali per zona
  LocaliDaCaricare = case NodeName of
                       "worker1@" ++ _ -> %% NORD
                         io:format(">> Loading NORD configuration...~n"),
                         [
                           {1, "Golden Pub", "Pub", 15.0, {18, 24}},
                           {2, "Pizza Nord", "Pizzeria", 20.0, {19, 23}}
                         ];
                       "worker2@" ++ _ -> %% CENTRO
                         io:format(">> Loading CENTRO configuration...~n"),
                         [
                           {3, "Ristorante Roma", "Ristorante", 35.0, {12, 15}},
                           {4, "Bar Centrale", "Bar", 10.0, {07, 20}}
                         ];
                       "worker3@" ++ _ -> %% SUD
                         io:format(">> Loading SUD configuration...~n"),
                         [
                           {5, "Bella Napoli", "Pizzeria", 18.0, {19, 24}},
                           {6, "Sud Bar", "Bar", 12.0, {08, 22}}
                         ];
                       _ ->
                         io:format(">> Nodo sconosciuto, nessun locale caricato.~n"),
                         []
                     end,

  %% Carichiamo i locali scelti
  lists:foreach(fun({Id, Nome, Tipo, Prezzo, Orari}) ->
    db_manager:add_locale(Id, Nome, Tipo, Prezzo, Orari)
                end, LocaliDaCaricare),
  %% ---------------------------------------------

  io:format(">> Attore Remoto pronto su ~p.~n", [node()]),
  register(vincolo_service, self()),
  loop(#state{total_users = 0}).

loop(State) ->
  receive
    {nuovo_vincolo, Id, IdEvento, Email, OraInizio, OraFine, BudMin, BudMax, TipoPreferito, _Pos} ->
      io:format("--- ELABORAZIONE VINCOLO ID: ~p ---~n", [Id]),
      NewTotalUsers = State#state.total_users + 1,
      Locali = db_manager:get_locali(),
      lists:foreach(fun(L) -> processa_locale(IdEvento, L, {OraInizio, OraFine, BudMin, BudMax, TipoPreferito}, NewTotalUsers) end, Locali),
      loop(State#state{total_users = NewTotalUsers});

    {richiedi_parziale, Pid_coordinatore, E_id} ->
      io:format(">> Worker (~p): Richiesta ottimo per Evento ~p~n", [node(), E_id]),
      {atomic, RisultatoMnesia} = mnesia:transaction(fun() -> mnesia:read(best_solution, E_id) end),
      Risposta = case RisultatoMnesia of
                   [Record] -> Record;
                   []       -> empty
                 end,
      Pid_coordinatore ! {risposta_parziale, E_id, Risposta},
      loop(State);

    Other ->
      io:format("Messaggio sconosciuto: ~p~n", [Other]),
      loop(State)
  end.

processa_locale(IdEvento, Locale, {U_Start, U_End, BMin, BMax, TipoPref}, TotUsers) ->
  {locale, L_Id, _Nome, L_Tipo, L_Prezzo, L_Apertura, L_Chiusura} = Locale,

  QualitaUtente = utils:calcola_qualita_utente(L_Tipo, L_Prezzo, TipoPref, BMin, BMax),
  SlotSovrapposti = utils:calcola_slot_sovrapposti(U_Start, U_End, L_Apertura, L_Chiusura),

  %% Aggiorniamo le statistiche
  {atomic, {NewAvgQual, MaxSlotCount}} = db_manager:update_stats(L_Id, QualitaUtente, SlotSovrapposti, TotUsers),

  %% --- 2. RECUPERIAMO L'ORA MIGLIORE ---
  %% Dobbiamo chiedere al DB qual è l'ora corrispondente al picco di partecipanti
  BestHour = db_manager:get_best_hour(L_Id),
  %% -------------------------------------

  Itot = utils:calcola_itot(NewAvgQual, MaxSlotCount, TotUsers, L_Id),

  %% Passiamo BestHour alla funzione di aggiornamento
  check_and_update_best(IdEvento, L_Id, Itot, BestHour).

%% Modificata per accettare BestHour
check_and_update_best(E_id, LocaleId, Itot, BestHour) ->
  F = fun() ->
    CurrentBest = mnesia:read(best_solution, E_id),
    Update = case CurrentBest of
               [] -> true;
               [#best_solution{score=OldScore}] when Itot > OldScore -> true;
               _ -> false
             end,

    if Update ->
      [LocaleRec] = mnesia:read(locale, LocaleId),
      NomeDelLocale = LocaleRec#locale.nome,

      io:format(">>> NUOVO RECORD! Locale ~p (~p) Score ~.4f Orario ~p~n", [LocaleId, NomeDelLocale, Itot, BestHour]),

      %% --- 3. SCRIVIAMO ORA E SCORE CORRETTI ---
      mnesia:write(#best_solution{
        id_evento = E_id,
        id_locale = LocaleId,
        nome_locale = NomeDelLocale,
        ora_inizio = BestHour,  %% <--- Qui salviamo l'ora reale (es. 20)
        score = Itot});         %% <--- Qui salviamo lo score reale
      true -> ok
    end
      end,
  mnesia:transaction(F).