-module(db_manager).
-export([init/0, add_locale/5, get_locali/0, update_stats/4, get_best_hour/1]).

-include("shared.hrl").

init() ->
  mnesia:create_schema([node()]),
  mnesia:start(),

  mnesia:create_table(locale, [{attributes, record_info(fields, locale)}]),
  mnesia:create_table(stats_locale, [{attributes, record_info(fields, stats_locale)}]),
  mnesia:create_table(slot_temporale, [{attributes, record_info(fields, slot_temporale)}]),
  mnesia:create_table(best_solution, [{attributes, record_info(fields, best_solution)}]),
  mnesia:create_table(global_state, [{attributes, record_info(fields, global_state)}]),

  mnesia:wait_for_tables([locale, stats_locale, slot_temporale, best_solution, global_state], 5000),

  io:format(">> Mnesia Database Inizializzato e Pronto.~n").

add_locale(Id, Nome, Tipo, Prezzo, {Apertura, Chiusura}) ->
  F = fun() ->
    mnesia:write(#locale{id=Id, nome=Nome, tipo=Tipo, prezzo_medio=Prezzo, apertura=Apertura, chiusura=Chiusura}),
    mnesia:write(#stats_locale{id_locale=Id, somma_qualita=0.0, num_utenti=0})
      end,
  mnesia:transaction(F).

get_locali() ->
  F = fun() -> mnesia:match_object(#locale{_='_'}) end,
  {atomic, Res} = mnesia:transaction(F),
  Res.

update_stats(LocaleId, PunteggioQualita, OreSovrapposte, TotaleUtentiAggiornato) ->
  F = fun() ->
    [Stats] = mnesia:read(stats_locale, LocaleId),
    NuovaSomma = Stats#stats_locale.somma_qualita + PunteggioQualita,
    NuoviUtenti = Stats#stats_locale.num_utenti + 1,
    mnesia:write(Stats#stats_locale{somma_qualita=NuovaSomma, num_utenti=NuoviUtenti}),

    lists:foreach(fun(Ora) ->
      Key = {LocaleId, Ora},
      case mnesia:read(slot_temporale, Key) of
        [] -> mnesia:write(#slot_temporale{key=Key, count=1});
        [S] -> mnesia:write(S#slot_temporale{count=S#slot_temporale.count + 1})
      end
                  end, OreSovrapposte),

    {NuovaSomma / NuoviUtenti, get_max_slot_count(LocaleId)}
      end,
  mnesia:transaction(F).

get_max_slot_count(LocaleId) ->
  Slots = mnesia:match_object(#slot_temporale{key={LocaleId, '_'}, _='_'}),
  Counts = [C || #slot_temporale{count=C} <- Slots],
  case Counts of
    [] -> 0;
    _ -> lists:max(Counts)
  end.

get_best_hour(LocaleId) ->
  {atomic, Res} = mnesia:transaction(fun() ->
    %% CORREZIONE: Usiamo la tabella 'slot_temporale' invece di 'statistiche_orarie'
    %% Il pattern cerca tutti i record che hanno come chiave {LocaleId, QualsiasiOra}
    Pattern = #slot_temporale{key = {LocaleId, '_'}, _ = '_'},

    Stats = mnesia:match_object(Pattern),

    %% Iteriamo sui risultati per trovare l'ora con il conteggio più alto
    lists:foldl(fun(Record, {BestOra, MaxC}) ->
      %% Il record è definito come {slot_temporale, Key, Count}
      %% Dove Key è {LocaleId, Ora}
      #slot_temporale{key = {_, Ora}, count = Count} = Record,

      if Count > MaxC -> {Ora, Count};
        true -> {BestOra, MaxC}
      end
                end, {0, -1}, Stats) %% {0, -1} sono i valori iniziali (Ora 0, Count -1)
                                     end),

  %% Res sarà la tupla {MigliorOra, MigliorCount}, restituiamo solo l'ora
  {BestH, _} = Res,
  BestH.