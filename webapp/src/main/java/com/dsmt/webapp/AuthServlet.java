package com.dsmt.webapp;

import com.fasterxml.jackson.databind.ObjectMapper;
import jakarta.servlet.ServletException;
import jakarta.servlet.annotation.WebServlet;
import jakarta.servlet.http.HttpServlet;
import jakarta.servlet.http.HttpServletRequest;
import jakarta.servlet.http.HttpServletResponse;
import jakarta.servlet.http.HttpSession;

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
        // Retrieve action parameter
        String action = request.getParameter("action");

        // If action is logout, handle logout
        if ("logout".equals(action)) {
                // Retrieve the current session, if it exists
                HttpSession session = request.getSession(false);

                if (session != null) {
                    // Invalidate the session to log out the user
                    session.invalidate();
                }

                request.setAttribute("success", "Logout effettuato con successo. A presto!");

                // Forward to login page after logout
                request.getRequestDispatcher("login.jsp").forward(request, response);
                return;
        }

        // Default action: show login page
        request.getRequestDispatcher("login.jsp").forward(request, response);
    }

    @Override
    protected void doPost(HttpServletRequest request, HttpServletResponse response) throws ServletException, IOException {
        String action = request.getParameter("action");
        String targetUrl;
        Map<String, String> jsonData = new HashMap<>();

        String errorPage = "login.jsp";

        // Retrieve email and password from request
        String email = request.getParameter("email");
        String psw = request.getParameter("psw");

        // If action is register, prepare registration data
        if ("register".equals(action)) {
            targetUrl = BACKEND_BASE_URL + "/register";
            errorPage = "registration.jsp"; // Se fallisce la reg, torniamo al form di reg

            String name = request.getParameter("nome");
            String surname = request.getParameter("cognome");

            jsonData.put("nome", name);
            jsonData.put("cognome", surname);
            jsonData.put("email", email);
            jsonData.put("psw", psw);

        } else if ("login".equals(action)) {
            // If action is login, prepare login data
            targetUrl = BACKEND_BASE_URL + "/login";
            errorPage = "login.jsp";

            jsonData.put("email", email);
            jsonData.put("psw", psw);
        } else {
            // Action not recognized
            response.sendError(HttpServletResponse.SC_BAD_REQUEST, "Azione non valida");
            return;
        }

        // Convert data to JSON string and send request
        String jsonInputString = new ObjectMapper().writeValueAsString(jsonData);
        HttpResult result = sendJsonRequest(targetUrl, jsonInputString);

        // Handle response: if success, redirect; if error, show error message
        if (result.statusCode == 200 || result.statusCode == 201) {
            if ("login".equals(action)) {
                ObjectMapper mapper = new ObjectMapper();
                Map<String, Object> userData = mapper.readValue(result.responseBody, Map.class);

                String nome = (String) userData.get("nome");
                String cognome = (String) userData.get("cognome");
                String userEmail = (String) userData.get("email");

                request.getSession().setAttribute("user", userEmail);
                request.getSession().setAttribute("nome", nome);
                request.getSession().setAttribute("cognome", cognome);

                request.getRequestDispatcher("index.jsp").forward(request, response);
            } else {
                request.setAttribute("success", "Registrazione completata con successo! Ora puoi accedere.");
                request.getRequestDispatcher("login.jsp").forward(request, response);
            }
        } else {
            request.setAttribute("error", "Errore: " + result.responseBody);
            request.getRequestDispatcher(errorPage).forward(request, response);
        }
    }

    // Helper method to send JSON POST request
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

    // Helper class to hold HTTP response data
    private static class HttpResult {
        int statusCode;
        String responseBody;

        HttpResult(int statusCode, String responseBody) {
            this.statusCode = statusCode;
            this.responseBody = responseBody;
        }
    }
}