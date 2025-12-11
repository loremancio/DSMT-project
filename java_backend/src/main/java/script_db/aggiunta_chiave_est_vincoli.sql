-- 1. Aggiungi la colonna per l'ID dell'utente
ALTER TABLE vincoli
ADD COLUMN user_id INT;

-- 2. Crea il collegamento (Foreign Key) con la tabella degli utenti
-- Assicurati che 'users' sia il nome corretto della tua tabella utenti
ALTER TABLE vincoli
ADD CONSTRAINT fk_vincoli_user
FOREIGN KEY (user_id) REFERENCES user(id)
ON DELETE CASCADE; -- Se l'utente viene cancellato, cancella anche i suoi vincoli