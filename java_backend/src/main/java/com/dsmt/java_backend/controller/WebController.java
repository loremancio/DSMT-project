package com.dsmt.java_backend.controller;

import com.dsmt.java_backend.model.User;
import com.dsmt.java_backend.service.UserService;
import jakarta.servlet.http.HttpSession;
import lombok.RequiredArgsConstructor;
import org.springframework.stereotype.Controller; // Nota: @Controller, NON @RestController
import org.springframework.ui.Model;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.RequestParam;

@Controller // Importante: Questo gestisce le pagine HTML/JSP
@RequiredArgsConstructor
public class WebController {

    private final UserService userService;

    // --- VISUALIZZAZIONE PAGINE ---

    @GetMapping("/login")
    public String showLoginPage() {
        return "login"; // Cerca /WEB-INF/jsp/login.jsp
    }

    @GetMapping("/register")
    public String showRegisterPage() {
        return "registration"; // Cerca /WEB-INF/jsp/registration.jsp
    }

    @GetMapping("/")
    public String home(HttpSession session) {
        // Protezione: se non c'è utente, vai al login
        if (session.getAttribute("user") == null) {
            return "redirect:/login";
        }
        return "index"; // Cerca /WEB-INF/jsp/index.jsp
    }

    @GetMapping("/logout")
    public String logout(HttpSession session) {
        session.invalidate();
        return "redirect:/login";
    }

    // --- GESTIONE ACTION (POST) ---

    @PostMapping("/auth/login")
    public String handleLogin(@RequestParam String email,
                              @RequestParam String psw,
                              HttpSession session,
                              Model model) {
        try {
            // Chiamata DIRETTA al service (Niente più HTTP/JSON!)
            User user = userService.login(email, psw);

            // Salviamo solo il nome in sessione come volevi
            session.setAttribute("user", user.getEmail());
            session.setAttribute("nome", user.getNome());
            session.setAttribute("cognome", user.getCognome());

            return "redirect:/"; // Vai alla home
        } catch (IllegalStateException e) {
            // Se fallisce, ricarica la pagina login con errore
            model.addAttribute("error", e.getMessage());
            return "login";
        }
    }

    @PostMapping("/auth/register")
    public String handleRegister(@RequestParam String nome,
                                 @RequestParam String cognome,
                                 @RequestParam String email,
                                 @RequestParam String psw,
                                 Model model) {
        try {
            User newUser = new User();
            newUser.setNome(nome);
            newUser.setCognome(cognome);
            newUser.setEmail(email);
            newUser.setPsw(psw);

            userService.register(newUser);

            model.addAttribute("success", "Registrazione OK! Ora accedi.");
            return "login"; // Vai al login
        } catch (IllegalStateException e) {
            model.addAttribute("error", e.getMessage());
            return "registration"; // Rimani sulla registrazione
        }
    }
}