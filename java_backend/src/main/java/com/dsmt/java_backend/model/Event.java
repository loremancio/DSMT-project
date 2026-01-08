package com.dsmt.java_backend.model;

import jakarta.persistence.*;
import lombok.*;

import java.time.Instant;
import java.util.HashSet;
import java.util.Set;
import java.util.List;
import java.time.LocalDateTime;

@Data
@Entity

@NoArgsConstructor
@AllArgsConstructor


@Table(name = "event") // Specifica il nome della tabella se non coincide con il nome della classe
public class Event {

    @Id
    @GeneratedValue(strategy = GenerationType.IDENTITY)
    private Integer id;

    // --- Relazione con la chiave esterna (creatore_id) ---
    // Mappa la colonna 'creatore_id' del DB all'oggetto User nel Model

    @ManyToOne
    @JoinColumn(name = "creatore_id", nullable = false)
    private User creatore;



    @Column(name = "privato")
    private Boolean isPrivato;

    @Column(name = "nome")
    private String nome;
    @ManyToMany
    @JoinTable(
            name = "event_partecipanti", // Nome della tabella di join nel DB
            joinColumns = @JoinColumn(name = "event_id"), // Colonna FK per questa entità (Event)
            inverseJoinColumns = @JoinColumn(name = "user_id") // Colonna FK per l'entità target (User)
    )


    private Set<User> partecipanti = new HashSet<>();


    @Column(name = "descrizione")
    private String descrizione;

    @Column(name = "deadline")
    private LocalDateTime deadline;

    @Column(name = "luogo_scelto")
    private String luogoScelto;

    @Column(name = "punteggio_finale")
    private Double punteggioFinale;

    @Column(name = "orario_scelto")
    private String orarioScelto;


}