package com.dsmt.java_backend.controller;

import com.dsmt.java_backend.service.EventService;
import dto.EventRequest;
import dto.EventResponse;
import jakarta.servlet.http.HttpSession;
import lombok.RequiredArgsConstructor;
import org.springframework.stereotype.Controller;
import org.springframework.ui.Model;
import org.springframework.web.bind.annotation.*;

import java.time.LocalDateTime;
import java.util.Collections;
import java.util.List;

@Controller
@RequestMapping("/events")
@RequiredArgsConstructor
public class EventController {

    private final EventService eventService;

    @PostMapping("/add")
    public String createEvent(@ModelAttribute EventRequest eventRequest, HttpSession session) {
        String emailUser = (String) session.getAttribute("user");
        if (emailUser == null) return "redirect:/login";

        eventRequest.setEmail_creatore(emailUser);

        eventService.addEvent(eventRequest);

        return "redirect:/";
    }

    @GetMapping("/view")
    public String viewEvents(@RequestParam(defaultValue = "all") String type,
                             @RequestParam(required = false) Long id,
                             Model model, HttpSession session) {

        if (session.getAttribute("user") == null) return "redirect:/login";

        Long user_id = (Long) session.getAttribute("id");
        List<EventResponse> risultati = Collections.emptyList();
        String messaggio = "";

        switch (type) {
            case "all":
                risultati = eventService.getAllEvents();
                messaggio = "Tutti gli Eventi";
                break;
            case "public":
                risultati = eventService.getAllPublicEvents();
                messaggio = "Eventi Pubblici";
                break;
            case "future":
                risultati = eventService.getAllFuturePublicEvents();
                messaggio = "Eventi Pubblici Futuri";
                break;
            case "byId":
                if (id != null) {
                    risultati = eventService.getEventById(id).map(List::of).orElse(Collections.emptyList());
                    messaggio = "Ricerca per ID: " + id;
                }
                break;
            case "myPrivate":
                risultati = eventService.getAllMyPrivateEvents(user_id);
                messaggio = "I Miei Eventi Privati (Partecipante)";
                break;

            case "myFuturePrivate":
                risultati = eventService.getAllMyPrivateFutureEvents(user_id);
                messaggio = "Miei Eventi Privati Futuri";
                break;
        }

        model.addAttribute("listaEventi", risultati);
        model.addAttribute("titoloTabella", messaggio);

        return "index";
    }
}