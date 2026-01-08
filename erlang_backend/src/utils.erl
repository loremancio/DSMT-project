-module(utils).
-export([calcola_qualita_utente/5, calcola_itot/4, calcola_slot_sovrapposti/4]).

%% COSTANTI
-define(W_SIM, 0.6).     %% Peso somiglianza
-define(W_BUDGET, 0.4).  %% Peso budget
-define(W_PART, 0.5).    %% Peso Partecipazione
-define(W_QUAL, 0.5).    %% Peso Qualità

%% --- Calcolo Indice Qualità Singolo Utente (Pqualità_k) ---
%% Pqualità = Wsim * Isim + Wbudget * Ibudget
calcola_qualita_utente(TipoLocale, PrezzoLocale, TipoPreferito, BudMin, BudMax) ->
  ISim = get_similarity(TipoLocale, TipoPreferito),
  io:format(">> Similarità tra ~p e ~p: ~.2f~n", [TipoLocale, TipoPreferito, ISim]),

  IBudget = get_budget_index(PrezzoLocale, BudMin, BudMax),

  io:format(">> Indice di Budget per prezzo ~.2f in range [~p - ~p]: ~.2f~n",
    [PrezzoLocale, BudMin, BudMax, IBudget]),

  (ISim * ?W_SIM) + (IBudget * ?W_BUDGET).

%% --- Matrice di Somiglianza ---
get_similarity("Ristorante", "Pub") -> 0.2;
get_similarity("Pub", "Ristorante") -> 0.2;
get_similarity("Ristorante", "Fast Food") -> 0.2;
get_similarity("Fast Food", "Ristorante") -> 0.2;
get_similarity("Ristorante", "Cocktail Bar") -> 0.15;
get_similarity("Cocktail Bar", "Ristorante") -> 0.15;
get_similarity("Ristorante", "Teatro") -> 0.1;
get_similarity("Teatro", "Ristorante") -> 0.1;
get_similarity("Ristorante", "Gelateria") -> 0.15;
get_similarity("Gelateria", "Ristorante") -> 0.15;
get_similarity("Ristorante", "Sala Giochi") -> 0.05;
get_similarity("Sala Giochi", "Ristorante") -> 0.05;
get_similarity("Pub", "Fast Food") -> 0.4;
get_similarity("Fast Food", "Pub") -> 0.4;
get_similarity("Pub", "Cocktail Bar") -> 0.7;
get_similarity("Cocktail Bar", "Pub") -> 0.7;
get_similarity("Pub", "Teatro") -> 0.1;
get_similarity("Teatro", "Pub") -> 0.1;
get_similarity("Pub", "Gelateria") -> 0.1;
get_similarity("Gelateria", "Pub") -> 0.1;
get_similarity("Pub", "Sala Giochi") -> 0.5;
get_similarity("Sala Giochi", "Pub") -> 0.5;
get_similarity("Fast Food", "Cocktail Bar") -> 0.1;
get_similarity("Cocktail Bar", "Fast Food") -> 0.1;
get_similarity("Fast Food", "Teatro") -> 0.05;
get_similarity("Teatro", "Fast Food") -> 0.05;
get_similarity("Fast Food", "Gelateria") -> 0.5;
get_similarity("Gelateria", "Fast Food") -> 0.5;
get_similarity("Fast Food", "Sala Giochi") -> 0.7;
get_similarity("Sala Giochi", "Fast Food") -> 0.7;
get_similarity("Cocktail Bar", "Teatro") -> 0.3;
get_similarity("Teatro", "Cocktail Bar") -> 0.3;
get_similarity("Cocktail Bar", "Gelateria") -> 0.1;
get_similarity("Gelateria", "Cocktail Bar") -> 0.1;
get_similarity("Cocktail Bar", "Sala Giochi") -> 0.2;
get_similarity("Sala Giochi", "Cocktail Bar") -> 0.2;
get_similarity("Teatro", "Gelateria") -> 0.1;
get_similarity("Gelateria", "Teatro") -> 0.1;
get_similarity("Teatro", "Sala Giochi") -> 0.05;
get_similarity("Sala Giochi", "Teatro") -> 0.05;
get_similarity("Gelateria", "Sala Giochi") -> 0.6;
get_similarity("Sala Giochi", "Gelateria") -> 0.6;

get_similarity(X, X) -> 1.0; %% Stesso tipo
get_similarity(_, _) -> 0.1. %% Default basso

%% --- Indice di Budget ---
get_budget_index(CostoMedio, BMin, BMax) ->
  Centro = (BMin + BMax) / 2,
  Margine = (BMax - BMin) / 2,

  if
  %% Scenario 1: Dentro il range
    CostoMedio >= BMin, CostoMedio =< BMax ->
      Diff = abs(CostoMedio - Centro),
      1 - ((Diff / Margine) * 0.5);

  %% Scenario 2/3: Fuori range
    true ->
      PuntoRif = if CostoMedio < BMin -> BMin; true -> BMax end,
      Diff = abs(CostoMedio - PuntoRif),
      Val = 0.5 - ((Diff / Margine) * 0.5),
      if Val < 0 -> 0.0; true -> Val end
  end.

%% --- Calcolo Slot Sovrapposti  ---
%% Ritorna una lista di ore (interi) in cui l'utente si sovrappone al locale
calcola_slot_sovrapposti(InizioUtente, FineUtente, AperturaLocale, ChiusuraLocale) ->
  StartU = trunc(InizioUtente),
  EndU = trunc(FineUtente),
  OpenL = trunc(AperturaLocale),
  CloseL = trunc(ChiusuraLocale),

  MaxStart = max(StartU, OpenL),
  MinEnd = min(EndU, CloseL),

  if
    MaxStart < MinEnd -> lists:seq(MaxStart, MinEnd - 1);
    true -> []
  end.

%% --- Calcolo Itot Finale ---
calcola_itot(AvgQuality, MaxPartecipantiSlot, TotaleUtentiSistema, _LocaleId) ->
  IPartecipazione = if TotaleUtentiSistema == 0 -> 0;
                      true -> MaxPartecipantiSlot / TotaleUtentiSistema
                    end,

  (IPartecipazione * ?W_PART) + (AvgQuality * ?W_QUAL).