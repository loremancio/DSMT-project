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

import java.time.LocalDateTime;
import java.util.List;
import java.util.Optional;

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
    @GetMapping("getAllEvents")
    public List<Event> getAllEvents() {
        return eventService.getAllEvents();
    }
    @GetMapping("getEventById")
    public ResponseEntity<Event> getEventById(@RequestParam long id) {
        return eventService.getEventById(id)
                .map(ResponseEntity::ok)
                .orElse(ResponseEntity.notFound().build());

    }
    @GetMapping("getAllPublicEvents")
    public List<Event> getAllPublicEvents(){
        return eventService.getAllPublicEvents();
    }
    @GetMapping("getAllFuturePublicEvents")
    public List<Event> getAllFuturePublicEvents(){
        return eventService.getAllFuturePublicEvents();
    }
    @GetMapping("getAllMyPrivateEvents")
    public List<Event> getAllMyPrivateEvents(@RequestParam Long user_id){
        return eventService.getAllMyPrivateEvents(user_id);
    }
    @GetMapping("getAllMyPrivateFutureEvents")
    public List<Event> getAllMyPrivateFutureEvents(@RequestParam Long user_id){
        return eventService.getAllMyPrivateFutureEvents(user_id);
    }
}
