package com.dsmt.java_backend.repository;

import com.dsmt.java_backend.model.Event;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.data.jpa.repository.Query;
import org.springframework.data.repository.query.Param;

import java.time.LocalDate;
import java.time.LocalDateTime;
import java.util.List;
import java.util.Optional;
public interface EventRepository extends JpaRepository<Event, Integer> {
    List<Event> findByIsPrivatoFalse();

    @Query("SELECT e FROM Event e WHERE e.isPrivato = false AND e.deadline >=:adesso")
    List<Event> findEventiPubbliciFuturi(@Param("adesso") LocalDateTime adesso);


    @Query("SELECT e FROM Event e join e.partecipanti ep where ep.id = :myID and e.isPrivato = true")
    List <Event> getAllMyPrivateEvents(@Param("myID")Integer myID);

    @Query("SELECT e FROM Event e join e.partecipanti ep where ep.id = :myID and e.isPrivato = true and e.deadline >=:adesso")
    List <Event> getAllMyPrivateFutureEvents(@Param("adesso") LocalDateTime adesso, @Param("myID")Integer myID);

    List<Event> findByDeadlineBetween(LocalDateTime start, LocalDateTime end);
}
