package dto;
import jakarta.persistence.Column;

import java.time.LocalDateTime;
import java.util.List;

public class EventRequest {
    private Long id;
    private Long creatore_id;
    private Boolean isPrivato;
    private String nome;
    private List<Long> partecipantiIds;
    private String descrizione;
    private LocalDateTime deadline;
}
