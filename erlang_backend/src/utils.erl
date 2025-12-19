-module(utils).
-export([calcola_qualita_utente/5, calcola_itot/4, calcola_slot_sovrapposti/4]).

%% COSTANTI (Pesi definiti nel documento)
-define(W_SIM, 0.6).     %% Peso somiglianza (Ipotesi)
-define(W_BUDGET, 0.4).  %% Peso budget (Ipotesi)
-define(W_PART, 0.5).    %% Peso Partecipazione [cite: 9]
-define(W_QUAL, 0.5).    %% Peso Qualità [cite: 9]

%% --- 1. Calcolo Indice Qualità Singolo Utente (Pqualità_k) ---
%% [cite: 18] Pqualità = Wsim * Isim + Wbudget * Ibudget
calcola_qualita_utente(TipoLocale, PrezzoLocale, TipoPreferito, BudMin, BudMax) ->
  ISim = get_similarity(TipoLocale, TipoPreferito),
  io:format(">> Similarità tra ~p e ~p: ~.2f~n", [TipoLocale, TipoPreferito, ISim]),

  IBudget = get_budget_index(PrezzoLocale, BudMin, BudMax),

  %% FIX: Uso ~p per BudMin e BudMax perché sono interi (10, 20) e ~.2f li fa crashare.
  %% Uso ~.2f solo per PrezzoLocale e IBudget che sono sicuramente float.
  io:format(">> Indice di Budget per prezzo ~.2f in range [~p - ~p]: ~.2f~n",
    [PrezzoLocale, BudMin, BudMax, IBudget]),

  (ISim * ?W_SIM) + (IBudget * ?W_BUDGET).

%% --- 2. Matrice di Somiglianza [cite: 50] ---
get_similarity("Pub", "Birreria") -> 0.9;
get_similarity("Birreria", "Pub") -> 0.9;
get_similarity("Ristorante", "Pub") -> 0.2;
get_similarity("Pub", "Ristorante") -> 0.2;
get_similarity(X, X) -> 1.0; %% Stesso tipo
get_similarity(_, _) -> 0.1. %% Default basso

%% --- 3. Indice di Budget [cite: 56-81] ---
get_budget_index(CostoMedio, BMin, BMax) ->
  Centro = (BMin + BMax) / 2,     %% [cite: 61]
  Margine = (BMax - BMin) / 2,    %% [cite: 62]

  if
  %% Scenario 1: Dentro il range [cite: 66]
    CostoMedio >= BMin, CostoMedio =< BMax ->
      Diff = abs(CostoMedio - Centro),
      1 - ((Diff / Margine) * 0.5);

  %% Scenario 2/3: Fuori range [cite: 71]
    true ->
      PuntoRif = if CostoMedio < BMin -> BMin; true -> BMax end, %% [cite: 75]
      Diff = abs(CostoMedio - PuntoRif),
      Val = 0.5 - ((Diff / Margine) * 0.5),
      if Val < 0 -> 0.0; true -> Val end %% [cite: 81]
  end.

%% --- 4. Calcolo Slot Sovrapposti  ---
%% Ritorna una lista di ore (interi) in cui l'utente si sovrappone al locale
calcola_slot_sovrapposti(InizioUtente, FineUtente, AperturaLocale, ChiusuraLocale) ->
  %% FIX: Convertiamo tutto in interi (trunc) perché Java manda float (es. 14.0)
  %% lists:seq crasha se riceve float.
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

%% --- 5. Calcolo Itot Finale [cite: 9, 28] ---
calcola_itot(AvgQuality, MaxPartecipantiSlot, TotaleUtentiSistema, _LocaleId) ->
  IPartecipazione = if TotaleUtentiSistema == 0 -> 0;
                      true -> MaxPartecipantiSlot / TotaleUtentiSistema
                    end, %% [cite: 24]

  (IPartecipazione * ?W_PART) + (AvgQuality * ?W_QUAL).