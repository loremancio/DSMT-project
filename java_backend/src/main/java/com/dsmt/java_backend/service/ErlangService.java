package com.dsmt.java_backend.service;

import com.dsmt.java_backend.model.Vincolo;
import com.dsmt.java_backend.repository.EventRepository;
import com.ericsson.otp.erlang.*;
import jakarta.annotation.PostConstruct;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;
import java.io.IOException;

@Service
public class ErlangService {

    private static final String REMOTE_NODE_NAME = "coordinator_node@10.2.1.39";
    private static final String COOKIE = "kairos";
    private static final String MAILBOX = "coordinator_service";
    private OtpNode javaNode = null;
    private OtpMbox mbox;

    @Autowired
    private EventRepository eventRepository;

    public ErlangService() throws IOException {

    }

    @PostConstruct
    public void init() {

        try {

            this.javaNode = new OtpNode("java_backend_node@10.2.1.39", COOKIE);
            mbox = javaNode.createMbox("java_mailbox");

            System.out.println(">> Nodo Java avviato. Avvio Receiver...");


            ErlangReceiver receiver = new ErlangReceiver(mbox, eventRepository);

            Thread t = new Thread(receiver);
            t.setDaemon(true);
            t.start();

        } catch (IOException e) {
            e.printStackTrace();
        }
    }

    public void sendTimerRequest(Integer id_evento, long delay){
        // Creiamo la tupla: {schedule_optimum, EventId, Delay}
        OtpErlangObject[] msgElements = new OtpErlangObject[3];

        msgElements[0] = new OtpErlangAtom("deadline");


        msgElements[1] = new OtpErlangInt(id_evento);


        msgElements[2] = new OtpErlangLong(delay);

        OtpErlangTuple msg = new OtpErlangTuple(msgElements);


         mbox.send(MAILBOX, REMOTE_NODE_NAME, msg);
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
        System.out.println("Vincolo triggerGlobalOptimum");

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