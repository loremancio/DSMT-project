package com.dsmt.java_backend.controller;
import com.dsmt.java_backend.model.User;
import com.dsmt.java_backend.service.UserService;
import lombok.RequiredArgsConstructor;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.*;

class LoginRequest {
    public String email;
    public String psw;
}
@RestController
@RequestMapping("/api/test")
@RequiredArgsConstructor
public class UserController {
    private final UserService userService;
    @GetMapping("/")
    public String prova(){
        return "first return";
    }
    @PostMapping
    public User createUser(@RequestBody User user){
        // aggiungere i controll
        return userService.createUser(user);
    }
    @PostMapping("/register")
    public ResponseEntity<?> registerUser(@RequestBody User user){
        try{
            User registeredUser = userService.register(user);
            System.out.println("reg successful");
            return new ResponseEntity<>(registeredUser, HttpStatus.CREATED);
        }catch(IllegalStateException e){
            System.out.println("reg unsuccessful");
            return new ResponseEntity<>(e.getMessage(), HttpStatus.BAD_REQUEST);
        }
    }
    @PostMapping("/login")
    public ResponseEntity<?> login(@RequestBody LoginRequest loginRequest){
        try{
            User autenticatedUser = userService.login(loginRequest.email, loginRequest.psw);
            System.out.println("login successful");
            return new ResponseEntity<>(autenticatedUser, HttpStatus.OK);
        }catch(IllegalStateException e){
            System.out.println("login failed");
            return new ResponseEntity<>(e.getMessage(), HttpStatus.UNAUTHORIZED);
        }
    }

}
