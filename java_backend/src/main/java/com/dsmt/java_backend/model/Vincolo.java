package com.dsmt.java_backend.model;

import jakarta.persistence.*;
import lombok.Data;
import lombok.NoArgsConstructor;
import lombok.AllArgsConstructor;

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

    // --- CAMPI DATI SEMPLIFICATI ---

    @Column(name = "ora_inizio")
    private Float oraInizio; // Es. 14.30

    @Column(name = "ora_fine")
    private Float oraFine;   // Es. 18.00

    @Column(name = "tipo_luogo")
    private String tipoLuogo;

    @Column(name = "budget_min")
    private Integer budgetMin; // Es. 15 (niente decimali)

    @Column(name = "budget_max")
    private Integer budgetMax; // Es. 30 (niente decimali)

    @Column(name = "posizione")
    private String posizione;
}