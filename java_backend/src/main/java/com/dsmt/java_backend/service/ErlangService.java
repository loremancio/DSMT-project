package com.dsmt.java_backend.service;

import com.dsmt.java_backend.model.Vincolo;
import com.ericsson.otp.erlang.*;
import org.springframework.stereotype.Service;
import java.io.IOException;

@Service
public class ErlangService {

    private static final String REMOTE_NODE_NAME = "server_node@LAPTOP-12PQ5VNM";
    private static final String COOKIE = "secret123";
    private static final String MAILBOX = "vincolo_service";
    private OtpNode javaNode = null;

    public ErlangService() throws IOException {
        // Creiamo il nodo una sola volta all'avvio del servizio
        this.javaNode = new OtpNode("java_backend_node", COOKIE);
    }

    public void sendVincolo(Vincolo v) {

        try {

            OtpMbox mbox = javaNode.createMbox();

            OtpErlangObject[] payload = new OtpErlangObject[]{
                    new OtpErlangAtom("nuovo_vincolo"),

                    new OtpErlangInt(v.getId()),

                    new OtpErlangInt(v.getEvent().getId()),

                    new OtpErlangString(v.getUser().getEmail()),

                    new OtpErlangDouble(v.getOraInizio() != null ? v.getOraInizio() : 0.0),
                    new OtpErlangDouble(v.getOraFine() != null ? v.getOraFine() : 0.0),

                    new OtpErlangInt(v.getBudgetMin() != null ? v.getBudgetMin() : 0),
                    new OtpErlangInt(v.getBudgetMax() != null ? v.getBudgetMax() : 0),

                    new OtpErlangString(v.getTipoLuogo() != null ? v.getTipoLuogo() : "Any"),
                    new OtpErlangString(v.getPosizione() != null ? v.getPosizione() : "Any")
            };

            OtpErlangTuple msg = new OtpErlangTuple(payload);

            if (javaNode.ping(REMOTE_NODE_NAME, 2000)) {
                mbox.send(MAILBOX, REMOTE_NODE_NAME, msg);
                System.out.println("Vincolo inviato a Erlang [ID: " + v.getId() + "]");
            } else {
                System.err.println("Erlang non raggiungibile.");
            }

        } catch (Exception e) {
            e.printStackTrace();
        }
    }
    public void triggerGlobalOptimum(Integer eventId) {

        try {

            OtpMbox mbox = javaNode.createMbox();

            // Prepariamo il messaggio: {calcola_ottimo_globale, EventId}
            OtpErlangObject[] payload = new OtpErlangObject[]{
                    new OtpErlangAtom("calcola_ottimo_globale"),
                    new OtpErlangLong(eventId)
            };
            OtpErlangTuple msg = new OtpErlangTuple(payload);

            if (javaNode.ping(REMOTE_NODE_NAME, 2000)) {
                mbox.send(MAILBOX, REMOTE_NODE_NAME, msg);
                System.out.println("Richiesta Ottimo Globale inviata per Evento: " + eventId);
            } else {
                System.err.println("Nodo Erlang Master non raggiungibile per la deadline.");
            }
        } catch (Exception e) {
            e.printStackTrace();
        }
    }

}