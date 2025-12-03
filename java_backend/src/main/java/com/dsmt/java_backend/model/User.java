package com.dsmt.java_backend.model;

import jakarta.persistence.*;

import lombok.Data;

@Data
@Entity

public class User {
    @Id
    @GeneratedValue(strategy = GenerationType.IDENTITY)

    private Long id;
    private String email;
    private String psw;
    private String nome;
    private String cognome;
}
