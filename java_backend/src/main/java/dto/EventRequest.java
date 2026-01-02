package dto;

import lombok.Data;
import lombok.NoArgsConstructor;
import lombok.AllArgsConstructor;
import java.time.LocalDateTime;
import java.util.List;

@Data // Genera automaticamente Getter, Setter, toString, equals e hashCode
@NoArgsConstructor // Genera un costruttore vuoto (fondamentale per Spring/Jackson)
@AllArgsConstructor // Genera un costruttore con tutti gli argomenti (comodo per i test)
public class EventRequest {

    private String emailCreatore;
    private Boolean isPrivato;
    private String nome;
    private List<String> mailPartecipanti;
    private String descrizione;
    private LocalDateTime deadline;
}