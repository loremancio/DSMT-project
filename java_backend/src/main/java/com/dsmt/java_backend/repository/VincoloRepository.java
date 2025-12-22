package com.dsmt.java_backend.repository;// VincoloRepository.java
import com.dsmt.java_backend.model.Vincolo;
import org.springframework.data.jpa.repository.JpaRepository;

public interface VincoloRepository extends JpaRepository<Vincolo, Integer> {
    // Potrebbe servirti in futuro: "Dammi tutti i vincoli di un evento"
    // List<Vincolo> findByEventoId(Long eventId);
}