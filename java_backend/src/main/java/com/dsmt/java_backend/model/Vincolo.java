package com.dsmt.java_backend.model;

import jakarta.persistence.*;
import lombok.*;

@Entity
@Data
@NoArgsConstructor
@AllArgsConstructor


@Table(name = "vincoli")
public class Vincolo {

    @Id
    @GeneratedValue(strategy = GenerationType.IDENTITY)
    private Integer id;

    // --- RELAZIONI ---
    @ManyToOne
    @JoinColumn(name = "user_id", nullable = false)
    private User user;

    @ManyToOne
    @JoinColumn(name = "event_id", nullable = false)
    private Event event;



    @Column(name = "ora_inizio")
    private Float oraInizio;

    @Column(name = "ora_fine")
    private Float oraFine;

    @Column(name = "tipo_luogo")
    private String tipoLuogo;

    @Column(name = "budget_min")
    private Integer budgetMin;

    @Column(name = "budget_max")
    private Integer budgetMax;

    @Column(name = "posizione")
    private String posizione;
}