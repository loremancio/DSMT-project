package com.dsmt.java_backend.service;

import com.dsmt.java_backend.model.Event;
import com.dsmt.java_backend.model.User;
import com.dsmt.java_backend.model.Vincolo;
import com.dsmt.java_backend.repository.EventRepository;
import com.dsmt.java_backend.repository.UserRepository;
import com.dsmt.java_backend.repository.VincoloRepository;
import dto.VincoloRequest;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional; // Importante!
import com.dsmt.java_backend.service.ErlangService;

@Service
public class VincoloService {

    @Autowired
    private VincoloRepository vincoloRepository;

    @Autowired
    private EventRepository eventRepository;

    @Autowired
    private UserRepository userRepository;

    @Autowired
    private ErlangService erlangService;

    @Transactional
    public Vincolo aggiungiVincolo(VincoloRequest dto) {

        Event evento = eventRepository.findById(dto.getIdEvento())
                .orElseThrow(() -> new RuntimeException("Evento non trovato con ID: " + dto.getIdEvento()));

        User utente = userRepository.findByEmail(dto.getEmailUtente())
                .orElseThrow(() -> new RuntimeException("Utente non trovato con email: " + dto.getEmailUtente()));

        boolean isCreatore = evento.getCreatore().getId().equals(utente.getId());

        boolean isPartecipante = evento.getPartecipanti().stream()
                .anyMatch(p -> p.getId().equals(utente.getId()));

        if (!isCreatore && !isPartecipante) {
            throw new RuntimeException("Non hai i permessi: devi essere il creatore o un partecipante per aggiungere vincoli.");
        }

        Vincolo nuovoVincolo = new Vincolo();
        nuovoVincolo.setEvent(evento);
        nuovoVincolo.setUser(utente);

        nuovoVincolo.setOraInizio(dto.getOraInizio());
        nuovoVincolo.setOraFine(dto.getOraFine());
        nuovoVincolo.setTipoLuogo(dto.getTipoLuogo());
        nuovoVincolo.setPosizione(dto.getPosizione());
        nuovoVincolo.setBudgetMin(dto.getBudgetMin());
        nuovoVincolo.setBudgetMax(dto.getBudgetMax());

        Vincolo vincoloSalvato = vincoloRepository.save(nuovoVincolo);

        erlangService.sendVincolo(vincoloSalvato);

        return vincoloSalvato;
    }
}