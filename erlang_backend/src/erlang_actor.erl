-module(erlang_actor).
-export([start/0, loop/1]).
-record(state, {total_users = 0}).

-include("shared.hrl").

start() ->
  db_manager:init(),

  %% --- 1. CARICAMENTO DIFFERENZIATO PER NODO (Configurazione PISA) ---
  NodeName = atom_to_list(node()),

  %% Definiamo i locali per zona (5 per worker)
  LocaliDaCaricare = case NodeName of
                       "worker1@" ++ _ -> %% --- ZONA SANTA MARIA (Zona Torre/Uni) IDs 1-9 ---
                         [
                           {1, "L'Ostellino",        "Pub",        12.0, {11, 24}}, %% Panini famosi
                           {2, "Pizzeria Il Montino","Pizzeria",   15.0, {19, 23}}, %% Storica
                           {3, "Rist. Alle Bandierine","Ristorante",35.0, {19, 23}}, %% Spaghetteria
                           {4, "Filter Coffee Lab",  "Bar",         7.0, {08, 19}}, %% Colazioni moderne
                           {5, "Auelli",             "Ristorante", 28.0, {12, 15}}  %% Pranzo universitario
                         ];

                       "worker2@" ++ _ -> %% --- ZONA BORGO STRETTO (Centro Storico) IDs 10-19 ---
                         [
                           {10, "Osteria i Santi",   "Ristorante", 32.0, {19, 23}}, %% Turistico ma buono
                           {11, "Sottobosco Libri",  "Bar",        10.0, {10, 24}}, %% Bar letterario/Jazz
                           {12, "Pizzeria Le Mura",  "Pizzeria",   18.0, {19, 23}}, %% Cena classica
                           {13, "Chupiteria",        "Pub",        15.0, {21, 03}}, %% Movida notturna
                           {14, "Argini e Margini",  "Pub",        14.0, {18, 01}}  %% Estivo sul fiume
                         ];

                       "worker3@" ++ _ -> %% --- ZONA STAZIONE / CORSO ITALIA IDs 20-29 ---
                         [
                           {20, "Orzo Bruno",        "Pub",        16.0, {18, 02}}, %% Birra artigianale famosa
                           {21, "Keith Art Cafe",    "Bar",         6.0, {07, 21}}, %% Vicino al Murales
                           {22, "Pizzeria Da Nando", "Pizzeria",   14.0, {12, 23}}, %% Economico e veloce
                           {23, "Ristorante La Scaletta","Ristorante", 55.0, {19, 23}}, %% Pesce di alto livello
                           {24, "Bar La Borsa",      "Bar",         5.0, {06, 20}}  %% Storico per caffè
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

  io:format(">> Attore Remoto pronto su ~p con ~p locali.~n", [node(), length(LocaliDaCaricare)]),
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