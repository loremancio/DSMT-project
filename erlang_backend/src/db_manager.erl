-module(db_manager).
-export([init/0, add_locale/5, get_locali/0, update_stats/4, get_best_hour/2, increment_total_users/1, find_best_locale_live/1]).

-include("shared.hrl").

%% Inizializza Mnesia e le tabelle necessarie
init() ->
  mnesia:create_schema([node()]),
  mnesia:start(),

  %% Usiamo disc_copies per mantenere i dati anche se il nodo si riavvia
  TableOptions = [{attributes, record_info(fields, locale)}, {disc_copies, [node()]}],
  StatsOptions = [{attributes, record_info(fields, stats_locale)}, {disc_copies, [node()]}],
  SlotOptions  = [{attributes, record_info(fields, slot_temporale)}, {disc_copies, [node()]}],
  BestOptions  = [{attributes, record_info(fields, best_solution)}, {disc_copies, [node()]}],
  GlobalOptions= [{attributes, record_info(fields, global_state)}, {disc_copies, [node()]}],

  mnesia:create_table(locale, TableOptions),
  mnesia:create_table(stats_locale, StatsOptions),
  mnesia:create_table(slot_temporale, SlotOptions),
  mnesia:create_table(best_solution, BestOptions),
  mnesia:create_table(global_state, GlobalOptions),

  %% Attendi che le tabelle siano pronte prima di procedere
  mnesia:wait_for_tables([locale, stats_locale, slot_temporale, best_solution, global_state], 5000),

  io:format(">> Mnesia Database Inizializzato.~n").

%% Aggiunge un nuovo locale al database
%% Parametri:
%% - Id: Identificativo univoco del locale
%% - Nome: Nome del locale
%% - Tipo: Tipo di locale (es. "Ristorante", "Bar", ecc.)
%% - Prezzo: Prezzo medio del locale
%% - Orari: Tupla {Apertura, Chiusura} che indica gli orari di apertura
add_locale(Id, Nome, Tipo, Prezzo, {Apertura, Chiusura}) ->
  F = fun() ->
    mnesia:write(#locale{id=Id, nome=Nome, tipo=Tipo, prezzo_medio=Prezzo, apertura=Apertura, chiusura=Chiusura})
      end,
  mnesia:transaction(F).

%% Recupera tutti i locali dal database
get_locali() ->
  F = fun() -> mnesia:match_object(#locale{_='_'}) end,
  {atomic, Res} = mnesia:transaction(F),
  Res.

%% Aggiorna le statistiche per un locale specifico e per un determinato evento
%% Parametri:
%% - IdEvento: Identificativo dell'evento per cui si aggiornano le statistiche
%% - LocaleId: Identificativo del locale
%% - PunteggioQualita: Punteggio di qualità relativo al vincolo utente
%% - OreSovrapposte: Lista delle ore (interi) in cui c'è sovrapposizione tra vincolo e orari del locale
%% Ritorna:
%% - {MediaQualita, MaxSlotCount}: Tupla con la nuova media di qualità e il massimo conteggio di slot temporali
update_stats(IdEvento, LocaleId, PunteggioQualita, OreSovrapposte) ->
  F = fun() ->
    %% Chiave univoca per stats_locale: {IdEvento, LocaleId}
    Key = {IdEvento, LocaleId},

    %% Aggiorna le statistiche di qualità per il locale specifico e l'evento
    Stats = case mnesia:read(stats_locale, Key) of
              [] -> #stats_locale{key=Key, somma_qualita=0.0, num_utenti=0};
              [S] -> S
            end,

    %% Aggiorna somma qualità e numero utenti
    NuovaSomma = Stats#stats_locale.somma_qualita + PunteggioQualita,
    NuoviUtenti = Stats#stats_locale.num_utenti + 1,
    mnesia:write(Stats#stats_locale{somma_qualita=NuovaSomma, num_utenti=NuoviUtenti}),

    %% Aggiorna il conteggio di utenti per ogni slot temporale sovrapposto
    lists:foreach(fun(Ora) ->
      SlotKey = {IdEvento, LocaleId, Ora},
      case mnesia:read(slot_temporale, SlotKey) of
        [] -> mnesia:write(#slot_temporale{key=SlotKey, count=1});
        [SlotRec] -> mnesia:write(SlotRec#slot_temporale{count=SlotRec#slot_temporale.count + 1})
      end
                  end, OreSovrapposte),

    %% Ritorna la nuova media del punteggio per un determinato locale ed evento e il massimo conteggio di
    %% utenti in uno slot temporale per quel locale ed evento
    {NuovaSomma / NuoviUtenti, get_max_slot_count_internal(IdEvento, LocaleId)}
      end,
  mnesia:transaction(F).

%% Recupera il massimo conteggio di utenti in uno slot temporale per un dato locale ed evento
%% Parametri:
%% - IdEvento: Identificativo dell'evento
%% - LocaleId: Identificativo del locale
%% Ritorna:
%% - MaxCount: Massimo conteggio di utenti in uno slot temporale per quel locale ed evento
get_max_slot_count_internal(IdEvento, LocaleId) ->
  %% Cerca pattern {IdEvento, LocaleId, QualsiasiOra}
  Pattern = #slot_temporale{key={IdEvento, LocaleId, '_'}, _='_'},
  Slots = mnesia:match_object(Pattern),
  Counts = [C || #slot_temporale{count=C} <- Slots],
  case Counts of
    [] -> 0;
    _ -> lists:max(Counts)
  end.

%% Recupera l'ora con il massimo conteggio di utenti per un dato locale ed evento
%% Parametri:
%% - IdEvento: Identificativo dell'evento
%% - LocaleId: Identificativo del locale
%% Ritorna:
%% - BestH: Ora con il massimo conteggio di utenti per quel locale ed evento
get_best_hour(IdEvento, LocaleId) ->
  {atomic, Res} = mnesia:transaction(fun() ->
    Pattern = #slot_temporale{key = {IdEvento, LocaleId, '_'}, _ = '_'},
    Stats = mnesia:match_object(Pattern),
    lists:foldl(fun(#slot_temporale{key={_, _, Ora}, count=C}, {BestOra, MaxC}) ->
      if C > MaxC -> {Ora, C}; true -> {BestOra, MaxC} end
                end, {0, -1}, Stats)
                                     end),
  {BestH, _} = Res,
  BestH.

%% Incrementa il contatore totale di utenti per un dato evento
%% Parametri:
%% - IdEvento: Identificativo dell'evento
%% Ritorna:
%% - Count: Nuovo conteggio totale di utenti per quell'evento
increment_total_users(IdEvento) ->
  F = fun() ->
    %% La chiave del contatore ora include l'IdEvento
    Key = {total_users, IdEvento},
    case mnesia:read(global_state, Key) of
      [] ->
        mnesia:write(#global_state{key=Key, value=1}),
        1;
      [#global_state{value=V}] ->
        NewV = V + 1,
        mnesia:write(#global_state{key=Key, value=NewV}),
        NewV
    end
      end,
  {atomic, Count} = mnesia:transaction(F),
  Count.

%% Trova il miglior locale live e restituisce anche il numero di utenti supportati (Hits)
%% Ritorna: {BestSolutionRecord, MaxSlotsCount} oppure {empty, 0}
find_best_locale_live(IdEvento) ->
  F = fun() ->
    Locali = mnesia:match_object(#locale{_='_'}),

    %% TotUsers LOCALE (serve per il calcolo dell'Itot relativo al nodo)
    TotUsers = case mnesia:read(global_state, {total_users, IdEvento}) of
                 [#global_state{value=V}] -> V;
                 [] -> 0
               end,

    if TotUsers == 0 -> {empty, 0};
      true ->
        %% Accumulatore ora è una tupla: {MigliorRecord, MigliorHits}
        lists:foldl(fun(LocaleRec, {CurrentBestRec, CurrentBestHits}) ->
          L_Id = LocaleRec#locale.id,
          Nome = LocaleRec#locale.nome,

          StatsKey = {IdEvento, L_Id},
          case mnesia:read(stats_locale, StatsKey) of
            [] -> {CurrentBestRec, CurrentBestHits};
            [Stats] ->
              AvgQual = Stats#stats_locale.somma_qualita / Stats#stats_locale.num_utenti,

              %% Questo è il "PESO" (Hits) che ci serve passare al Coordinator
              MaxSlots = get_max_slot_count_internal(IdEvento, L_Id),

              %% Calcoliamo lo score locale
              Itot = utils:calcola_itot(AvgQual, MaxSlots, TotUsers, L_Id),

              %% Troviamo l'ora migliore (solo per costruire il record)
              PatternH = #slot_temporale{key = {IdEvento, L_Id, '_'}, _ = '_'},
              SlotsH = mnesia:match_object(PatternH),
              {BestHour, _} = lists:foldl(fun(#slot_temporale{key={_, _, Ora}, count=C}, {BestOra, MaxC}) ->
                if C > MaxC -> {Ora, C}; true -> {BestOra, MaxC} end
                                          end, {0, -1}, SlotsH),

              NewRec = #best_solution{id_evento=IdEvento, id_locale=L_Id, nome_locale=Nome, ora_inizio=BestHour, score=Itot},

              %% Logica di confronto (Maximization)
              case CurrentBestRec of
                empty ->
                  {NewRec, MaxSlots}; %% Primo candidato trovato
                #best_solution{score=MaxScore} when Itot > MaxScore ->
                  {NewRec, MaxSlots}; %% Trovato candidato migliore
                _ ->
                  {CurrentBestRec, CurrentBestHits} %% Teniamo il precedente
              end
          end
                    end, {empty, 0}, Locali) %% Inizializziamo accumulatore a {empty, 0}
    end
      end,
  {atomic, Result} = mnesia:transaction(F),
  Result.