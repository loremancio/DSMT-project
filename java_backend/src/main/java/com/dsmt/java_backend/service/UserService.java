package com.dsmt.java_backend.service;


import com.dsmt.java_backend.model.User;
import com.dsmt.java_backend.repository.UserRepository;
import lombok.RequiredArgsConstructor;
import org.springframework.stereotype.Service;

import java.nio.charset.StandardCharsets;
import java.security.MessageDigest;
import java.security.NoSuchAlgorithmException;
import java.util.Objects;
import java.util.Optional;

@Service
@RequiredArgsConstructor
public class UserService {
    private final UserRepository userRepository;

    private String hashPassword(String plainPassword) {
        try {
            MessageDigest digest = MessageDigest.getInstance("SHA-256");
            byte[] encodedHash = digest.digest(plainPassword.getBytes(StandardCharsets.UTF_8));

            StringBuilder hexString = new StringBuilder();
            for (byte b : encodedHash) {
                String hex = Integer.toHexString(0xff & b);
                if (hex.length() == 1) {
                    hexString.append('0');
                }
                hexString.append(hex);
            }
            return hexString.toString();
        } catch (NoSuchAlgorithmException e) {
            throw new RuntimeException("Errore durante l'hashing della password", e);
        }
    }
    public User createUser(User user) {
        user.setPsw(hashPassword(user.getPsw()));
        return userRepository.save(user);
    }
    /**
     * Registra un nuovo utente nel database.
     * @param user L'oggetto utente da registrare.
     * @return L'utente salvato.
     * @throws IllegalStateException Se l'email è già in uso.
     */
    public User register(User user){
        if(userRepository.existsByEmail(user.getEmail())){
            throw new IllegalStateException("Email già registrata");
        }

        String hashPassword = hashPassword(user.getPsw());
        user.setPsw(hashPassword);
        return userRepository.save(user);
    }
    /**
     * Verifica le credenziali dell'utente per il login.
     * @param email Email dell'utente.
     * @return L'utente se le credenziali sono valide.
     * @throws IllegalStateException Se le credenziali non sono valide.
     */
    public User login (String email, String password){
        Optional<User> userOptional = userRepository.findByEmail(email);
        if(userOptional.isEmpty()) // non so se va ho modificato un po di roba
            throw new IllegalStateException("Credenziali non valide");
        User user = userOptional.get();

        String hashedInputPassword = hashPassword(password);
        if(Objects.equals(user.getPsw(), hashedInputPassword)){
            return user;
        }else {
            throw new IllegalStateException("Credenziali non valide");
        }
    }
}