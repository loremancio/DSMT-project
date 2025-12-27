package com.dsmt.java_backend.service;

import com.dsmt.java_backend.model.Event;
import com.dsmt.java_backend.model.User;
import com.dsmt.java_backend.repository.EventRepository;
import com.dsmt.java_backend.repository.UserRepository;
import dto.EventRequest;
import dto.EventResponse;
import io.micrometer.observation.ObservationFilter;
import lombok.RequiredArgsConstructor;

import org.springframework.stereotype.Service;

import java.time.LocalDateTime;
import java.util.ArrayList;
import java.util.List;
import java.util.Objects;
import java.util.Optional;
import java.util.stream.Collectors;

@Service
@RequiredArgsConstructor
public class EventService {
    private final EventRepository eventRepository;
    private final UserRepository userRepository;



    public Event addEvent(EventRequest dto) {
        Event newEvent = new Event();
        newEvent.setNome(dto.getNome());
        newEvent.setDescrizione(dto.getDescrizione());
        newEvent.setIsPrivato(dto.getIsPrivato());
        newEvent.setDeadline(dto.getDeadline());

        // mappo la gestione creatore da string a oggetto User
        User CreatoreTrovato = userRepository.findByEmail(dto.getEmail_creatore())
                .orElseThrow(() -> new RuntimeException("User not found"));
        newEvent.setCreatore(CreatoreTrovato);

        newEvent.getPartecipanti().add(CreatoreTrovato);

        // gestione partecipanti da List<String> a Set<User>
        if(dto.getMail_partecipanti() != null)
            for(String mail: dto.getMail_partecipanti())
            {
                User partecipanti = userRepository.findByEmail(mail)
                        .orElseThrow(() -> new RuntimeException("User not found"));
                newEvent.getPartecipanti().add(partecipanti);
            }


        return eventRepository.save(newEvent);
    }

    private List<EventResponse> conversione(List<Event> e){
        List<EventResponse> listaRisposte = new ArrayList<>();
        for (Event event : e) {
            EventResponse dto = new EventResponse();
            dto.setId(event.getId());
            dto.setNome(event.getNome());
            dto.setDescrizione(event.getDescrizione());
            dto.setIsPrivato(event.getIsPrivato());
            dto.setDeadline(event.getDeadline());
            dto.setEmail_creatore(event.getCreatore().getEmail());
            dto.setLuogoScelto(event.getLuogoScelto());
            List<String> emails = event.getPartecipanti().stream()
                    .map(user -> user.getEmail())
                    .collect(Collectors.toList());
            dto.setMail_partecipanti(emails);
            listaRisposte.add(dto);
        }
        return listaRisposte;
    }
    private EventResponse conversione_singolo(Event event){
        EventResponse dto = new EventResponse();
        dto.setId(event.getId());
        dto.setNome(event.getNome());
        dto.setDescrizione(event.getDescrizione());
        dto.setIsPrivato(event.getIsPrivato());
        dto.setDeadline(event.getDeadline());
        dto.setEmail_creatore(event.getCreatore().getEmail());
        dto.setLuogoScelto(event.getLuogoScelto());
        List<String> emails = event.getPartecipanti().stream()
                .map(user -> user.getEmail())
                .collect(Collectors.toList());
        dto.setMail_partecipanti(emails);
        return dto;
    }

    public List<EventResponse> getAllEvents() {
        List<Event> eventi =  eventRepository.findAll();
        List<EventResponse> listaRisposte = new ArrayList<>();
        for (Event event : eventi) {
            EventResponse dto = new EventResponse();
            dto.setId(event.getId());
            dto.setNome(event.getNome());
            dto.setDescrizione(event.getDescrizione());
            dto.setIsPrivato(event.getIsPrivato());
            dto.setDeadline(event.getDeadline());
            dto.setEmail_creatore(event.getCreatore().getEmail());
            dto.setLuogoScelto(event.getLuogoScelto());
            List<String> emails = event.getPartecipanti().stream()
                                        .map(user -> user.getEmail())
                                        .collect(Collectors.toList());
            dto.setMail_partecipanti(emails);
            listaRisposte.add(dto);
        }
        return listaRisposte;
    }

    public Optional<EventResponse> getEventById(Integer id) {
         /*Visto che il tuo metodo restituisce già un Optional<EventResponse>
          possiamo usare la funzione .map(). È molto elegante:
          trasforma l'oggetto dentro l'Optional solo se esiste; se non esiste,
           restituisce un Optional vuoto automaticamente.
         * */
        return eventRepository.findById(id)
                .map(event -> conversione_singolo(event));
    }
    public List<EventResponse> getAllPublicEvents(){
        List<Event> eventi =  eventRepository.findByIsPrivatoFalse();
        List<EventResponse> listaRisposte = new ArrayList<>();
        listaRisposte.addAll(conversione(eventi));
        return listaRisposte;
    }
    public List<EventResponse> getAllFuturePublicEvents(){

        return conversione(eventRepository.findEventiPubbliciFuturi(LocalDateTime.now()));
    }
    public List<EventResponse> getAllMyPrivateEvents(Integer user_id){
        return conversione(eventRepository.getAllMyPrivateEvents(user_id));
    }
    public List<EventResponse> getAllMyPrivateFutureEvents(Integer user_id){
        return conversione(eventRepository.getAllMyPrivateFutureEvents(LocalDateTime.now(), user_id));
    }
}
