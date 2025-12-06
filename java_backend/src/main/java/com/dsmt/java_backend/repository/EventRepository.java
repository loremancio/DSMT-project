package com.dsmt.java_backend.repository;

import com.dsmt.java_backend.model.Event;
import org.springframework.data.jpa.repository.JpaRepository;
import java.util.Optional;
public interface EventRepository extends JpaRepository<Event, Long> {

}
