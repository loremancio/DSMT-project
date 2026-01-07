-module(erlang_actor).
-export([start_link/0, init_internal/0, loop/1]).
-record(state, {}).
-include("shared.hrl").

-define(COORDINATOR_NODE, 'coordinator_node@10.2.1.39').

start_link() ->
  Pid = spawn_link(?MODULE, init_internal, []),
  {ok, Pid}.

init_internal() ->
  pg:start(pg),

  %% PARSING DEL NOME (worker_nord@... -> "nord")
  NodeStr = atom_to_list(node()),
  [NamePart | _] = string:split(NodeStr, "@"),
  ZoneStr = case string:prefix(NamePart, "worker_") of
              nomatch -> "generico";
              Z -> Z
            end,

  %% CALCOLO GRUPPI (Specifico + Macro)
  SpecificGroup = list_to_atom("gruppo_" ++ ZoneStr),
  MacroGroups =
    case string:find(ZoneStr, "nord") of nomatch -> []; _ -> ['gruppo_nord'] end ++
    case string:find(ZoneStr, "centro") of nomatch -> []; _ -> ['gruppo_centro'] end ++
    case string:find(ZoneStr, "sud") of nomatch -> []; _ -> ['gruppo_sud'] end,

  AllGroups = lists:usort([SpecificGroup | MacroGroups]),
  lists:foreach(fun(G) -> pg:join(G, self()) end, AllGroups),
  pg:join('tutti_i_worker', self()), %% Per la deadline globale

  try register(vincolo_service, self()) of true -> ok catch error:badarg -> ok end,
  db_manager:init(),

  io:format(">> [WORKER] Avviato su ~p (Zona: ~p).~n", [node(), ZoneStr]),

  %% Caricamento dati basato sulla zona (nord/centro/sud)
  LocaliDaCaricare = case ZoneStr of
                       "nord" ->
                         [{1, "L'Ostellino", "Pub", 12.0, {11, 24}},
                           {2, "Pizzeria Il Montino","Pizzeria", 15.0, {19, 23}},
                           {3, "Rist. Alle Bandierine","Ristorante",35.0, {19, 23}},
                           {4, "Filter Coffee Lab", "Bar", 7.0, {08, 19}},
                           {5, "Auelli", "Ristorante", 28.0, {12, 15}}];
                       "centro" ->
                         [{10, "Osteria i Santi", "Ristorante", 32.0, {19, 23}},
                           {11, "Sottobosco Libri", "Bar", 10.0, {10, 24}},
                           {12, "Pizzeria Le Mura", "Pizzeria", 18.0, {19, 23}},
                           {13, "Chupiteria", "Pub", 15.0, {21, 03}},
                           {14, "Argini e Margini", "Pub", 14.0, {18, 01}}];
                       "sud" ->
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

  %% Avvio loop di connessione
  self() ! check_connection,

  loop(#state{}).

loop(State) ->
  receive
  %% --- CONNESSIONE AL CLUSTER ---
    check_connection ->
      IsConnected = lists:member(?COORDINATOR_NODE, nodes()),
      if IsConnected -> ok;
        true ->
          case net_adm:ping(?COORDINATOR_NODE) of
            pong -> io:format(">> [CLUSTER] Connesso con successo al Coordinatore!~n");
            pang -> io:format(".") %% Puntino per attesa
          end
      end,
      erlang:send_after(5000, self(), check_connection),
      loop(State);

  %% --- NUOVO VINCOLO UTENTE (FIX: Aggiunto _Email) ---
    {nuovo_vincolo, Id, IdEvento, _Email, OraInizio, OraFine, BudMin, BudMax, TipoPreferito, _Pos} ->
      io:format("--- [WORKER ~p] VINCOLO ID: ~p PER EVENTO: ~p ---~n", [node(), Id, IdEvento]),

      TotalUsers = db_manager:increment_total_users(IdEvento),
      Locali = db_manager:get_locali(),

      lists:foreach(fun(L) ->
        processa_locale(IdEvento, L, {OraInizio, OraFine, BudMin, BudMax, TipoPreferito}, TotalUsers)
                    end, Locali),
      loop(State);

  %% --- RICHIESTA OTTIMO PARZIALE ---
    {richiedi_parziale, Pid_coordinatore, E_id} ->
      io:format(">> [WORKER ~p] Calcolo ottimo parziale per Evento ~p~n", [node(), E_id]),

      {BestRec, Hits} = db_manager:find_best_locale_live(E_id),

      case BestRec of
        empty -> ok;
        Rec -> io:format("   Invio candidato: ~p (Score Locale: ~.2f) con peso: ~p utenti~n",
          [Rec#best_solution.nome_locale, Rec#best_solution.score, Hits])
      end,

      Pid_coordinatore ! {risposta_parziale, E_id, {BestRec, Hits}},
      loop(State);

    Other ->
      io:format("Messaggio sconosciuto: ~p~n", [Other]),
      loop(State)
  end.

processa_locale(IdEvento, Locale, {U_Start, U_End, BMin, BMax, TipoPref}, TotUsers) ->
  {locale, L_Id, NomeLocale, L_Tipo, L_Prezzo, L_Apertura, L_Chiusura} = Locale,

  QualitaUtente = utils:calcola_qualita_utente(L_Tipo, L_Prezzo, TipoPref, BMin, BMax),
  SlotSovrapposti = utils:calcola_slot_sovrapposti(U_Start, U_End, L_Apertura, L_Chiusura),

  %% --- FIX: Rimosso TotUsers dalla chiamata (solo 4 argomenti ora) ---
  {atomic, {NewAvgQual, MaxSlotCount}} = db_manager:update_stats(IdEvento, L_Id, QualitaUtente, SlotSovrapposti),

  BestHour = db_manager:get_best_hour(IdEvento, L_Id),
  Itot = utils:calcola_itot(NewAvgQual, MaxSlotCount, TotUsers, L_Id),

  io:format("   -> [CALCOLO E:~p] ~p: Qual=~.2f, SlotMax=~p, TotUser=~p => ITOT = ~.4f~n",
    [IdEvento, NomeLocale, NewAvgQual, MaxSlotCount, TotUsers, Itot]),

  check_and_update_best(IdEvento, L_Id, NomeLocale, Itot, BestHour).

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