package dto;

import lombok.AllArgsConstructor;
import lombok.Data;
import lombok.NoArgsConstructor;

import java.time.LocalDateTime;
import java.util.List;

@Data
@NoArgsConstructor // Genera un costruttore vuoto
@AllArgsConstructor
public class EventResponse {
    private Integer id;
    private String nome;
    private String descrizione;
    private LocalDateTime deadline;
    private Boolean isPrivato;
    private String luogoScelto;
    private String orarioScelto;
    private Double punteggioFinale;


    private String emailCreatore;
    private List<String> mailPartecipanti;
}