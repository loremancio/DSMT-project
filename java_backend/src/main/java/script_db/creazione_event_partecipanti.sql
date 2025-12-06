CREATE TABLE event_partecipanti (
    event_id INT,
    user_id INT,
    data_iscrizione TIMESTAMP DEFAULT CURRENT_TIMESTAMP,
    stato VARCHAR(20) DEFAULT 'confermato', -- Es: confermato, in attesa, cancellato

    -- 1. Chiave Primaria Composta
    -- Evita che lo stesso utente si iscriva due volte allo stesso evento
    PRIMARY KEY (event_id, user_id),

    -- 2. Chiavi Esterne (Foreign Keys)
    CONSTRAINT fk_evento
        FOREIGN KEY (event_id) 
        REFERENCES event(id)
        ON DELETE CASCADE,  -- Se elimini l'evento, elimini le iscrizioni

    CONSTRAINT fk_utente
        FOREIGN KEY (user_id) 
        REFERENCES user(id)
        ON DELETE CASCADE   -- Se elimini l'utente, elimini le sue iscrizioni
);