package com.dsmt.java_backend.controller;

import com.dsmt.java_backend.service.VincoloService;
import dto.VincoloRequest;
import jakarta.servlet.http.HttpSession;
import lombok.RequiredArgsConstructor;
import org.springframework.stereotype.Controller;
import org.springframework.web.bind.annotation.ModelAttribute;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.servlet.mvc.support.RedirectAttributes;

@Controller
@RequestMapping("/vincoli")
@RequiredArgsConstructor
public class VincoloController {

    private final VincoloService vincoloService;

    @PostMapping("/add")
    public String aggiungiVincolo(@ModelAttribute VincoloRequest vincoloDto,
                                  HttpSession session,
                                  RedirectAttributes redirectAttributes) {

        String emailUser = (String) session.getAttribute("user");
        if (emailUser == null) return "redirect:/login";

        try {
            // Imposta l'utente loggato come autore del vincolo
            vincoloDto.setEmailUtente(emailUser);

            // Prova a salvare
            vincoloService.aggiungiVincolo(vincoloDto);

            // Se va bene, prepara messaggio verde
            redirectAttributes.addFlashAttribute("successMessage", "Vincolo salvato correttamente!");

        } catch (RuntimeException e) {
            // SE VA MALE (es. non partecipante), CATTURA L'ERRORE QUI
            // Prepara messaggio rosso
            redirectAttributes.addFlashAttribute("errorMessage", "Errore: " + e.getMessage());
        }

        // Torna alla home in ogni caso
        return "redirect:/";
    }
}