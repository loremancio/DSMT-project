-- Aggiornamento orari a FLOAT
ALTER TABLE vincoli MODIFY ora_inizio FLOAT;
ALTER TABLE vincoli MODIFY ora_fine FLOAT;

-- Aggiornamento budget a INT
ALTER TABLE vincoli MODIFY budget_min INT;
ALTER TABLE vincoli MODIFY budget_max INT;