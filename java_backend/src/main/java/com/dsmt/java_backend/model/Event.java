package com.dsmt.java_backend.model;

import jakarta.persistence.*;
import lombok.Data;

import java.util.HashSet;
import java.util.Set;
import java.util.List;
import java.time.LocalDateTime;

@Data
@Entity
@Table(name = "event") // Specifica il nome della tabella se non coincide con il nome della classe
public class Event {

    @Id
    @GeneratedValue(strategy = GenerationType.IDENTITY)
    private Long id;

    // --- Relazione con la chiave esterna (creatore_id) ---
    // Mappa la colonna 'creatore_id' del DB all'oggetto User nel Model

    @ManyToOne
    @JoinColumn(name = "creatore_id", nullable = false)
    private User creatore;
    //correggere mettere la email

    // Altri attributi diretti della tabella 'event'
    @Column(name = "privato")
    private Boolean isPrivato; // Uso un nome più standard per un campo booleano

    @Column(name = "nome")
    private String nome;
    @ManyToMany
    @JoinTable(
            name = "event_partecipanti", // Nome della tabella di join nel DB
            joinColumns = @JoinColumn(name = "event_id"), // Colonna FK per questa entità (Event)
            inverseJoinColumns = @JoinColumn(name = "user_id") // Colonna FK per l'entità target (User)
    )


    private Set<User> partecipanti = new HashSet<>();
    //correggere Set <String> email_partecipanti

    @Column(name = "descrizione")
    private String descrizione;

    @Column(name = "deadline")
    private LocalDateTime deadline; // Utilizzo LocalDateTime per una colonna data/ora

    // Nota: Non è necessario includere esplicitamente i getter/setter,
    // l'annotazione @Data di Lombok li gestisce automaticamente.
}