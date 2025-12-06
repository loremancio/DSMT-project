CREATE TABLE vincoli (
    id INT AUTO_INCREMENT PRIMARY KEY,
    ora_inizio TIME,
    ora_fine TIME,
    tipo_luogo VARCHAR(100),
    budget_min DECIMAL(10, 2),
    budget_max DECIMAL(10, 2),
    posizione VARCHAR(255),
    event_id INT NOT NULL,

    -- Definizione della chiave esterna che punta alla tabella 'event'
    CONSTRAINT fk_vincoli_evento
        FOREIGN KEY (event_id)
        REFERENCES event(id)
        ON DELETE CASCADE
);