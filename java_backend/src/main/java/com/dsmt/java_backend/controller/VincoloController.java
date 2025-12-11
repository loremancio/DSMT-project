package com.dsmt.java_backend.controller;

import com.dsmt.java_backend.model.Vincolo;
import com.dsmt.java_backend.service.VincoloService;
import dto.VincoloRequest;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.RequestBody;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RestController;

@RestController
@RequestMapping("/api/vincoli")
public class VincoloController {

    @Autowired
    private VincoloService vincoloService; // Usiamo il nuovo service

    @PostMapping
    public ResponseEntity<?> aggiungiVincolo(@RequestBody VincoloRequest vincoloDto) {
        try {
            Vincolo vincoloSalvato = vincoloService.aggiungiVincolo(vincoloDto);
            return ResponseEntity.ok("Vincolo aggiunto, ID: " + vincoloSalvato.getId());
        } catch (RuntimeException e) {
            return ResponseEntity.badRequest().body(e.getMessage());
        }
    }
}