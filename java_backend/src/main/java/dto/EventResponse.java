package dto;

import lombok.AllArgsConstructor;
import lombok.Data;
import lombok.NoArgsConstructor;

import java.time.LocalDateTime;
import java.util.List;

@Data
@NoArgsConstructor // Genera un costruttore vuoto (fondamentale per Spring/Jackson)
@AllArgsConstructor
public class EventResponse {
    private Integer id;
    private String nome;
    private String descrizione;
    private LocalDateTime deadline;
    private Boolean isPrivato;

    // Invece dell'oggetto User completo, restituiamo solo le email
    private String emailCreatore;
    private List<String> mailPartecipanti;
}