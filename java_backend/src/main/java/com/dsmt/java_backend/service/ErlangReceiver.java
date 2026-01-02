package com.dsmt.java_backend.service;

import com.dsmt.java_backend.repository.EventRepository;
import com.ericsson.otp.erlang.*;
import org.springframework.stereotype.Component;

public class ErlangReceiver implements Runnable {
    private final OtpMbox mbox;
    private final EventRepository eventRepository; // <--- NUOVO CAMPO
    private boolean running = true;

    public ErlangReceiver(OtpMbox mbox, EventRepository eventRepository) {
        this.mbox = mbox;
        this.eventRepository = eventRepository;
    }

    @Override
    public void run() {
        System.out.println(">> Java Receiver Thread avviato. In ascolto su: " + mbox.getName());

        while (running) {
            try {
                // Resta in attesa (bloccante) finché non arriva un messaggio
                OtpErlangObject message = mbox.receive();

                if (message instanceof OtpErlangTuple) {
                    processMessage((OtpErlangTuple) message);
                }
            } catch (OtpErlangExit | OtpErlangDecodeException e) {
                System.err.println("Errore nel thread di ricezione: " + e.getMessage());
                running = false;
            }
        }
    }

    private void processMessage(OtpErlangTuple msg) {
        // Messaggio: {risultato_finale, EventId, {best_solution, EvId, LocId, NomeLoc, Ora, Score}}
        try {
            OtpErlangAtom tag = (OtpErlangAtom) msg.elementAt(0);

            if ("risultato_finale".equals(tag.atomValue())) {
                // 1. Prendi l'ID Evento (Indice 1)
                OtpErlangLong eventIdObj = (OtpErlangLong) msg.elementAt(1);
                int eventId = (int) eventIdObj.longValue();

                // 2. Prendi la TUPLA della soluzione (Indice 2)
                OtpErlangTuple bestRecord = (OtpErlangTuple) msg.elementAt(2);

                // La struttura della tupla Erlang corrisponde al record:
                // #best_solution{id_evento, id_locale, nome_locale, ora_inizio, score}
                // Indici Tupla: 0=tag, 1=id_ev, 2=id_loc, 3=nome_loc, 4=ora, 5=score

                OtpErlangString nomeLocaleObj = (OtpErlangString) bestRecord.elementAt(3);
                String nomeLocale = nomeLocaleObj.stringValue();

                OtpErlangObject oraObj = bestRecord.elementAt(4);
                long oraInizio = (oraObj instanceof OtpErlangLong) ? ((OtpErlangLong) oraObj).longValue() : 0;
                String orarioFormattato = String.format("%02d:00 - %02d:00", oraInizio, oraInizio + 2);

                OtpErlangDouble scoreObj = (OtpErlangDouble) bestRecord.elementAt(5);
                double score = scoreObj.doubleValue();

                System.out.println(">>> [VINCITORE]: Evento " + eventId + " vince " + nomeLocale + " con score " + score + " alle ore " + oraInizio);

                System.out.println(">>> SALVATAGGIO SU DB: " + nomeLocale);

                // --- SALVATAGGIO SU DB ---
                salvaVincitore(eventId, nomeLocale, score, orarioFormattato);
            }
        } catch (Exception e) {
            e.printStackTrace();
        }
    }

    private void salvaVincitore(Integer eventId, String nome, double score, String orario) {
        // Nota: eventRepository.findById potrebbe richiedere una transazione o contesto JPA
        // Se hai problemi di sessione, usa TransactionTemplate o @Transactional sul Service chiamante
        try {
            eventRepository.findById(eventId).ifPresent(evento -> {
                evento.setLuogoScelto(nome);
                evento.setPunteggioFinale(score);
                evento.setOrarioScelto(orario);
                eventRepository.save(evento);
            });
        } catch (Exception e) {
            System.err.println("Errore salvataggio DB: " + e.getMessage());
        }
    }

    public void stop() {
        this.running = false;
    }
}