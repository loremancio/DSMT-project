package dto;

import lombok.Data;
import lombok.NoArgsConstructor;
import lombok.AllArgsConstructor;

@Data
@NoArgsConstructor
@AllArgsConstructor
public class VincoloRequest {

    private String emailUtente;
    private Integer idEvento;

    // Nuovi tipi semplificati
    private Float oraInizio;
    private Float oraFine;

    private String tipoLuogo;
    private String posizione;

    private Integer budgetMin;
    private Integer budgetMax;
}