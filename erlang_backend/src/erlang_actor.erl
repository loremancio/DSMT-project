-module(erlang_actor).
-export([start_link/0, init_internal/0, loop/1]).
-record(state, {}).
-include("shared.hrl").

%% Avvia il processo actor
start_link() ->
  Pid = spawn_link(?MODULE, init_internal, []),
  {ok, Pid}.

%% Inizializza il nodo worker
init_internal() ->
  try register(vincolo_service, self()) of
    true -> ok
  catch
    error:badarg -> ok
  end,

  %% Connessione al coordinatore
  net_adm:ping('coordinator_node@127.0.0.1'),
  io:format(">> [WORKER] Avviato su ~p.~n", [node()]),
  db_manager:init(),

  %% Caricamento dati locali in base al nodo
  NodeName = atom_to_list(node()),
  LocaliDaCaricare = case NodeName of
                       "worker1@" ++ _ ->
                         [{1, "L'Ostellino", "Pub", 12.0, {11, 24}},
                           {2, "Pizzeria Il Montino","Pizzeria", 15.0, {19, 23}},
                           {3, "Rist. Alle Bandierine","Ristorante",35.0, {19, 23}},
                           {4, "Filter Coffee Lab", "Bar", 7.0, {08, 19}},
                           {5, "Auelli", "Ristorante", 28.0, {12, 15}}];
                       "worker2@" ++ _ ->
                         [{10, "Osteria i Santi", "Ristorante", 32.0, {19, 23}},
                           {11, "Sottobosco Libri", "Bar", 10.0, {10, 24}},
                           {12, "Pizzeria Le Mura", "Pizzeria", 18.0, {19, 23}},
                           {13, "Chupiteria", "Pub", 15.0, {21, 03}},
                           {14, "Argini e Margini", "Pub", 14.0, {18, 01}}];
                       "worker3@" ++ _ ->
                         [{20, "Orzo Bruno", "Pub", 16.0, {18, 02}},
                           {21, "Keith Art Cafe", "Bar", 6.0, {07, 21}},
                           {22, "Pizzeria Da Nando", "Pizzeria", 14.0, {12, 23}},
                           {23, "Ristorante La Scaletta","Ristorante", 55.0, {19, 23}},
                           {24, "Bar La Borsa", "Bar", 5.0, {06, 20}}];
                       _ -> []
                     end,

  lists:foreach(fun({Id, Nome, Tipo, Prezzo, Orari}) ->
    db_manager:add_locale(Id, Nome, Tipo, Prezzo, Orari)
                end, LocaliDaCaricare),

  loop(#state{}).

%% Loop principale per la ricezione dei messaggi
loop(State) ->
  receive
    %% --- NUOVO VINCOLO UTENTE ---
    {nuovo_vincolo, Id, IdEvento, OraInizio, OraFine, BudMin, BudMax, TipoPreferito, _Pos} ->
      io:format("--- [WORKER ~p] VINCOLO ID: ~p PER EVENTO: ~p ---~n", [node(), Id, IdEvento]),

      %% Passiamo IdEvento per incrementare il contatore SPECIFICO
      TotalUsers = db_manager:increment_total_users(IdEvento),

      %% Recuperiamo tutti i locali
      Locali = db_manager:get_locali(),

      %% Aggiorniamo le statistiche per ogni locale
      lists:foreach(fun(L) ->
        processa_locale(IdEvento, L, {OraInizio, OraFine, BudMin, BudMax, TipoPreferito}, TotalUsers)
                    end, Locali),
      loop(State);

    %% --- RICHIESTA OTTIMO PARZIALE ---
    {richiedi_parziale, Pid_coordinatore, E_id} ->
      io:format(">> [WORKER ~p] Calcolo ottimo parziale per Evento ~p~n", [node(), E_id]),
      BestLive = db_manager:find_best_locale_live(E_id),
      Pid_coordinatore ! {risposta_parziale, E_id, BestLive},
      loop(State);

    Other ->
      io:format("Messaggio sconosciuto: ~p~n", [Other]),
      loop(State)
  end.

%% Calcola le statistiche per un singolo locale
%% Parametri:
%% - IdEvento: Identificativo dell'evento
%% - Locale: Record del locale
%% - {U_Start, U_End, BMin, BMax, TipoPref}: Vincolo utente
%% - TotUsers: Numero totale di utenti che hanno inviato vincoli per l'evento
processa_locale(IdEvento, Locale, {U_Start, U_End, BMin, BMax, TipoPref}, TotUsers) ->
  {locale, L_Id, NomeLocale, L_Tipo, L_Prezzo, L_Apertura, L_Chiusura} = Locale,

  %% Calcolo qualità utente e slot sovrapposti
  QualitaUtente = utils:calcola_qualita_utente(L_Tipo, L_Prezzo, TipoPref, BMin, BMax),
  SlotSovrapposti = utils:calcola_slot_sovrapposti(U_Start, U_End, L_Apertura, L_Chiusura),

  %% Aggiorna le statistiche nel database
  {atomic, {NewAvgQual, MaxSlotCount}} = db_manager:update_stats(IdEvento, L_Id, QualitaUtente, SlotSovrapposti, TotUsers),

  %% Recupera l'ora migliore per il locale ed evento
  BestHour = db_manager:get_best_hour(IdEvento, L_Id),

  %% Calcola ITOT: Indice di soddisfazione totale
  Itot = utils:calcola_itot(NewAvgQual, MaxSlotCount, TotUsers, L_Id),

  io:format("   -> [CALCOLO E:~p] ~p: Qual=~.2f, SlotMax=~p, TotUser=~p => ITOT = ~.4f~n",
    [IdEvento, NomeLocale, NewAvgQual, MaxSlotCount, TotUsers, Itot]),

  %% Controlla e aggiorna la migliore soluzione globale se necessario
  check_and_update_best(IdEvento, L_Id, NomeLocale, Itot, BestHour).

%% Controlla e aggiorna la migliore soluzione globale per un evento
%% Parametri:
%% - E_id: Identificativo dell'evento
%% - LocaleId: Identificativo del locale
%% - NomeLocale: Nome del locale
%% - Itot: Indice di soddisfazione totale calcolato
%% - BestHour: Ora di inizio migliore per il locale ed evento
check_and_update_best(E_id, LocaleId, NomeLocale, Itot, BestHour) ->
  F = fun() ->
    CurrentBest = mnesia:read(best_solution, E_id),

    Update = case CurrentBest of
               [] ->
                 io:format("      [NUOVO LEADER E:~p] ~p (Score: ~.4f)~n", [E_id, NomeLocale, Itot]),
                 true;
               [#best_solution{score=OldScore, nome_locale=OldName, id_locale=OldId}] ->
                 IsSameLeader = (LocaleId == OldId),
                 if
                   IsSameLeader ->
                     io:format("      [UPDATE LEADER E:~p] ~p: ~.4f -> ~.4f~n", [E_id, NomeLocale, OldScore, Itot]),
                     true;
                   Itot > OldScore ->
                     io:format("      [SORPASSO E:~p] ~p (~.4f) batte ~p (~.4f).~n", [E_id, NomeLocale, Itot, OldName, OldScore]),
                     true;
                   true -> false
                 end
             end,

    if Update ->
      mnesia:write(#best_solution{id_evento = E_id, id_locale = LocaleId, nome_locale = NomeLocale, ora_inizio = BestHour, score = Itot});
      true -> ok
    end
      end,
  mnesia:transaction(F).