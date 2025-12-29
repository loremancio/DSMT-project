package com.dsmt.java_backend.model;

import jakarta.persistence.*;

import lombok.*;

@Data
@Entity

@NoArgsConstructor
@AllArgsConstructor


public class User {
    @Id
    @GeneratedValue(strategy = GenerationType.IDENTITY)

    private Integer id;
    private String email;
    private String psw;
    private String nome;
    private String cognome;

}
