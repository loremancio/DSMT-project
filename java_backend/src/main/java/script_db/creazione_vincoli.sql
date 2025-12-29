CREATE TABLE vincoli (
                         id INT AUTO_INCREMENT PRIMARY KEY,
                         ora_inizio FLOAT,
                         ora_fine FLOAT,
                         tipo_luogo VARCHAR(100),
                         budget_min INT,
                         budget_max INT,
                         posizione VARCHAR(255),
                         event_id INT NOT NULL,
                         user_id INT not null ,

    -- Definizione della chiave esterna che punta alla tabella 'event'
                         CONSTRAINT fk_vincoli_evento
                             FOREIGN KEY (event_id)
                                 REFERENCES event(id)
                                 ON DELETE CASCADE,
                         CONSTRAINT fk_vincoli_user
                             FOREIGN KEY (user_id) REFERENCES User(id)
                                 ON DELETE CASCADE
);