package com.dsmt.java_backend.service;

import com.ericsson.otp.erlang.*;
import org.springframework.stereotype.Component;

public class ErlangReceiver implements Runnable {
    private final OtpMbox mbox;
    private boolean running = true;

    public ErlangReceiver(OtpMbox mbox) {
        this.mbox = mbox;
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
        // Supponiamo che il messaggio sia {risultato_finale, {IdLocale, Score}}
        String tag = ((OtpErlangAtom) msg.elementAt(0)).atomValue();

        if ("risultato_finale".equals(tag)) {
            OtpErlangObject content = msg.elementAt(1);
            System.out.println(">>> [RISULTATO RICEVUTO DA ERLANG]: " + content.toString());

            // Qui potresti iniettare un Service JPA per salvare su MySQL
            // esempio: myService.saveWinner(content);
        }
    }

    public void stop() {
        this.running = false;
    }
}