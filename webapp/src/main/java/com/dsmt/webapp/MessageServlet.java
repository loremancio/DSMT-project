package com.dsmt.webapp;

import java.io.IOException;
import java.net.URI;
import java.net.URLEncoder;
import java.net.http.HttpClient;
import java.net.http.HttpRequest;
import java.net.http.HttpResponse;
import java.nio.charset.StandardCharsets;

import jakarta.servlet.ServletException;
import jakarta.servlet.annotation.WebServlet;
import jakarta.servlet.http.HttpServlet;
import jakarta.servlet.http.HttpServletRequest;
import jakarta.servlet.http.HttpServletResponse;

@WebServlet(name = "messageServlet", value = "/messages")
public class MessageServlet extends HttpServlet {

    // Indirizzo del Backend Spring Boot
    private static final String BACKEND_URL = "http://localhost:8081/api/messages";

    private final HttpClient httpClient = HttpClient.newHttpClient();

    @Override
    protected void doGet(HttpServletRequest request, HttpServletResponse response) throws ServletException, IOException {
        String action = request.getParameter("action");

        if ("load".equals(action)) {
            try {
                HttpRequest apiRequest = HttpRequest.newBuilder()
                        .uri(URI.create(BACKEND_URL))
                        .GET()
                        .build();

                HttpResponse<String> apiResponse = httpClient.send(apiRequest, HttpResponse.BodyHandlers.ofString());

                // Controlliamo anche se il backend ci ha risposto picche (es. 404 o 500)
                if (apiResponse.statusCode() == 200) {
                    request.setAttribute("serverData", apiResponse.body());
                } else {
                    request.setAttribute("serverData", "Errore dal backend (Status " + apiResponse.statusCode() + ")");
                }

                // CORREZIONE: Catturiamo Exception generica per includere anche IOException (Connection Refused)
            } catch (Exception e) {
                request.setAttribute("serverData", "Impossibile connettersi al Backend: " + e.getMessage() +
                        ". Assicurati che Spring Boot sia avviato sulla porta 8081.");
            }
        }

        request.getRequestDispatcher("index.jsp").forward(request, response);
    }

    @Override
    protected void doPost(HttpServletRequest request, HttpServletResponse response) throws ServletException, IOException {
        String messageText = request.getParameter("text");

        if (messageText != null && !messageText.isEmpty()) {
            try {
                String encodedText = URLEncoder.encode(messageText, StandardCharsets.UTF_8);
                String addUrl = BACKEND_URL + "/add?text=" + encodedText;

                HttpRequest apiRequest = HttpRequest.newBuilder()
                        .uri(URI.create(addUrl))
                        .POST(HttpRequest.BodyPublishers.noBody())
                        .build();

                httpClient.send(apiRequest, HttpResponse.BodyHandlers.ofString());

            } catch (Exception e) {
                // In caso di errore nel salvataggio, stampiamo nello stack trace
                e.printStackTrace();
            }
        }

        response.sendRedirect("messages");
    }
}