package com.dsmt.java_backend.service;

import com.dsmt.java_backend.model.Event;
import com.dsmt.java_backend.repository.EventRepository;
import io.micrometer.observation.ObservationFilter;
import lombok.RequiredArgsConstructor;

import org.springframework.stereotype.Service;

import java.time.LocalDateTime;
import java.util.List;
import java.util.Objects;
import java.util.Optional;
@Service
@RequiredArgsConstructor
public class EventService {
    private final EventRepository eventRepository;



    public Event addEvent(Event newEvent) {
        return eventRepository.save(newEvent);
    }
    public List<Event> getAllEvents() {
        return eventRepository.findAll();
    }

    public Optional<Event> getEventById(long id) {
        return eventRepository.findById(id);
    }
    public List<Event> getAllPublicEvents(){
        return eventRepository.findByIsPrivatoFalse();
    }
    public List<Event> getAllFuturePublicEvents(){
        return eventRepository.findEventiPubbliciFuturi(LocalDateTime.now());
    }
    public List<Event> getAllMyPrivateEvents(Long user_id){
        return eventRepository.getAllMyPrivateEvents(user_id);
    }
    public List<Event> getAllMyPrivateFutureEvents(Long user_id){
        return eventRepository.getAllMyPrivateFutureEvents(LocalDateTime.now(), user_id);
    }
}
