package com.dsmt.java_backend.service; // O il tuo package service

import com.dsmt.java_backend.model.Event;
import com.dsmt.java_backend.model.User;
import com.dsmt.java_backend.model.Vincolo;
import com.dsmt.java_backend.repository.EventRepository;
import com.dsmt.java_backend.repository.UserRepository;
import com.dsmt.java_backend.repository.VincoloRepository;
import dto.VincoloRequest;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;

@Service
public class VincoloService {

    @Autowired
    private VincoloRepository vincoloRepository;

    @Autowired
    private EventRepository eventRepository; // Ci serve per controllare se l'evento esiste

    @Autowired
    private UserRepository userRepository;   // Ci serve per controllare l'utente

    public Vincolo aggiungiVincolo(VincoloRequest dto) {

        // 1. Recupera Evento
        Event evento = eventRepository.findById(dto.getIdEvento())
                .orElseThrow(() -> new RuntimeException("Evento non trovato con ID: " + dto.getIdEvento()));

        // 2. Recupera Utente
        User utente = userRepository.findByEmail(dto.getEmailUtente())
                .orElseThrow(() -> new RuntimeException("Utente non trovato con email: " + dto.getEmailUtente()));

        // 3. Logica di Business: Controllo Partecipazione
        if (!evento.getPartecipanti().contains(utente)) {
            throw new RuntimeException("L'utente non è tra i partecipanti dell'evento.");
        }

        // 4. Mappatura DTO -> Entity
        Vincolo nuovoVincolo = new Vincolo();
        nuovoVincolo.setEvent(evento);
        nuovoVincolo.setUser(utente);

        nuovoVincolo.setOraInizio(dto.getOraInizio());
        nuovoVincolo.setOraFine(dto.getOraFine());
        nuovoVincolo.setTipoLuogo(dto.getTipoLuogo());
        nuovoVincolo.setPosizione(dto.getPosizione());
        nuovoVincolo.setBudgetMin(dto.getBudgetMin());
        nuovoVincolo.setBudgetMax(dto.getBudgetMax());

        // 5. Salva
        return vincoloRepository.save(nuovoVincolo);
    }
}