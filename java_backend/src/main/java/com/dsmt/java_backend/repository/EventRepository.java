package com.dsmt.java_backend.repository;

import com.dsmt.java_backend.model.Event;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.data.jpa.repository.Query;
import org.springframework.data.repository.query.Param;

import java.time.LocalDateTime;
import java.util.List;
import java.util.Optional;
public interface EventRepository extends JpaRepository<Event, Long> {
    List<Event> findByIsPrivatoFalse();

    @Query("SELECT e FROM Event e WHERE e.isPrivato = false AND e.deadline >=:adesso")
    List<Event> findEventiPubbliciFuturi(@Param("adesso") LocalDateTime adesso);

    /* SELECT e FROM Event e: Seleziona oggetti Evento.
       JOIN e.partecipanti p: "Entra" nella lista dei partecipanti dell'evento (JPA sa già quale tabella usare grazie al @ManyToMany).
        WHERE p.id = :userId: ...dove tra i partecipanti c'è l'utente con questo ID.
        AND e.isPrivato = true: ...e l'evento è privato.
    */
    @Query("SELECT e FROM Event e  join e.partecipanti ep where ep.id = :myID and e.isPrivato = true")
    List <Event> getAllMyPrivateEvents(@Param("myID")Long myID);

    @Query("SELECT e FROM Event e  join e.partecipanti ep where ep.id = :myID and e.isPrivato = true and e.deadline >=:adesso")
    List <Event> getAllMyPrivateFutureEvents(@Param("adesso") LocalDateTime adesso, @Param("myID")Long myID);
}
