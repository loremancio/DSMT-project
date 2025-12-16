package com.dsmt.java_backend.service;

import com.dsmt.java_backend.model.Vincolo;
import com.ericsson.otp.erlang.*;
import org.springframework.stereotype.Service;
import java.io.IOException;

@Service
public class ErlangService {

    private static final String REMOTE_NODE_NAME = "server_node@PCDelLorenzo";
    private static final String COOKIE = "secret123";
    private static final String MAILBOX = "vincolo_service";

    public void sendVincolo(Vincolo v) {
        OtpNode javaNode = null;
        try {
            javaNode = new OtpNode("java_client_vincoli", COOKIE);
            OtpMbox mbox = javaNode.createMbox();

            OtpErlangObject[] payload = new OtpErlangObject[]{
                    new OtpErlangAtom("nuovo_vincolo"),
                    new OtpErlangInt(v.getId()),
                    new OtpErlangString(v.getUser().getEmail()),
                    new OtpErlangInt(v.getEvent().getId()),

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

        } catch (IOException e) {
            e.printStackTrace();
        } finally {
            if (javaNode != null) javaNode.close();
        }
    }
}