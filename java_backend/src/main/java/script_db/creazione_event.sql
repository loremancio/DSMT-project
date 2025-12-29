CREATE TABLE event (
    id INT AUTO_INCREMENT PRIMARY KEY,
    creatore_id INT NOT NULL,
    privato BOOLEAN DEFAULT FALSE,
    nome VARCHAR(255) NOT NULL,
    descrizione TEXT,
    deadline DATETIME,

    -- Definizione della chiave esterna
    CONSTRAINT fk_event_creatore
        FOREIGN KEY (creatore_id)
        REFERENCES User(id)
        ON DELETE CASCADE
);