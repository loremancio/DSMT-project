package com.dsmt.java_backend.service;

import com.dsmt.java_backend.model.Event;
import com.dsmt.java_backend.repository.EventRepository;
import lombok.RequiredArgsConstructor;
import org.springframework.stereotype.Service;

import java.util.Objects;
import java.util.Optional;
@Service
@RequiredArgsConstructor
public class EventService {
    private final EventRepository eventRepository;
    public Event addEvent(Event newEvent) {
        return eventRepository.save(newEvent);
    }
}
