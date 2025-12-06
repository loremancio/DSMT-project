package com.dsmt.java_backend.controller;
import com.dsmt.java_backend.model.Event;
import com.dsmt.java_backend.model.User;
import com.dsmt.java_backend.service.UserService;
import com.dsmt.java_backend.service.EventService;
import dto.EventRequest;
import lombok.RequiredArgsConstructor;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.*;

@RestController
@RequestMapping("/api")
@RequiredArgsConstructor
public class EventController {
    private final EventService eventService;
    @PostMapping("/addEvent")
    public Event createEvent(@RequestBody Event newEvent) {
        //User creatore = userRepository.findById().orElseThrow(() ->new RuntimeException("Creatore non trovato");

        return eventService.addEvent(newEvent);
    }
}
