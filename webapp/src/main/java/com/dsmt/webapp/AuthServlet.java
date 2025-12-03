package com.dsmt.webapp;

import com.fasterxml.jackson.databind.ObjectMapper;
import jakarta.servlet.ServletException;
import jakarta.servlet.annotation.WebServlet;
import jakarta.servlet.http.HttpServlet;
import jakarta.servlet.http.HttpServletRequest;
import jakarta.servlet.http.HttpServletResponse;

import java.io.BufferedReader;
import java.io.IOException;
import java.io.InputStreamReader;
import java.io.OutputStream;
import java.net.HttpURLConnection;
import java.net.URL;
import java.nio.charset.StandardCharsets;
import java.util.HashMap;
import java.util.Map;

@WebServlet(name = "AuthServlet", value = "/auth")
public class AuthServlet extends HttpServlet {

    private static final String BACKEND_BASE_URL = "http://localhost:8080/api/test";

    @Override
    protected void doGet(HttpServletRequest request, HttpServletResponse response) throws ServletException, IOException {
        // Default: manda al login
        request.getRequestDispatcher("login.jsp").forward(request, response);
    }

    @Override
    protected void doPost(HttpServletRequest request, HttpServletResponse response) throws ServletException, IOException {
        String action = request.getParameter("action");
        String targetUrl;
        Map<String, String> jsonData = new HashMap<>();

        // Variabili per capire dove tornare in caso di errore
        String errorPage = "login.jsp";

        String email = request.getParameter("email");
        String psw = request.getParameter("psw");

        if ("register".equals(action)) {
            targetUrl = BACKEND_BASE_URL + "/register";
            errorPage = "registration.jsp"; // Se fallisce la reg, torniamo al form di reg

            String nome = request.getParameter("nome");
            String cognome = request.getParameter("cognome");

            jsonData.put("nome", nome);
            jsonData.put("cognome", cognome);
            jsonData.put("email", email);
            jsonData.put("psw", psw);

        } else if ("login".equals(action)) {
            targetUrl = BACKEND_BASE_URL + "/login";
            errorPage = "login.jsp"; // Se fallisce il login, torniamo al login

            jsonData.put("email", email);
            jsonData.put("psw", psw);
        } else {
            response.sendError(HttpServletResponse.SC_BAD_REQUEST, "Azione non valida");
            return;
        }

        // Chiamata al Backend
        String jsonInputString = new ObjectMapper().writeValueAsString(jsonData);
        HttpResult result = sendJsonRequest(targetUrl, jsonInputString);

        if (result.statusCode == 200 || result.statusCode == 201) {
            // SUCCESSO
            if ("login".equals(action)) {
                request.getSession().setAttribute("user", result.responseBody);
                // Login riuscito -> vai alla Home
                request.getRequestDispatcher("index.jsp").forward(request, response);
            } else {
                // Registrazione riuscita -> vai al Login per accedere
                request.setAttribute("success", "Registrazione completata con successo! Ora puoi accedere.");
                request.getRequestDispatcher("login.jsp").forward(request, response);
            }
        } else {
            // ERRORE
            request.setAttribute("error", "Errore: " + result.responseBody);
            // Torna alla pagina specifica (login o registrazione)
            request.getRequestDispatcher(errorPage).forward(request, response);
        }
    }

    // --- Metodi Helper invariati ---
    private HttpResult sendJsonRequest(String urlString, String jsonInput) throws IOException {
        URL url = new URL(urlString);
        HttpURLConnection con = (HttpURLConnection) url.openConnection();
        con.setRequestMethod("POST");
        con.setRequestProperty("Content-Type", "application/json; utf-8");
        con.setRequestProperty("Accept", "application/json");
        con.setDoOutput(true);

        try (OutputStream os = con.getOutputStream()) {
            byte[] input = jsonInput.getBytes(StandardCharsets.UTF_8);
            os.write(input, 0, input.length);
        }

        int code = con.getResponseCode();
        StringBuilder response = new StringBuilder();
        try (BufferedReader br = new BufferedReader(
                new InputStreamReader((code >= 200 && code < 300) ? con.getInputStream() : con.getErrorStream(), StandardCharsets.UTF_8))) {
            String responseLine;
            while ((responseLine = br.readLine()) != null) {
                response.append(responseLine.trim());
            }
        }
        return new HttpResult(code, response.toString());
    }

    private static class HttpResult {
        int statusCode;
        String responseBody;

        HttpResult(int statusCode, String responseBody) {
            this.statusCode = statusCode;
            this.responseBody = responseBody;
        }
    }
}