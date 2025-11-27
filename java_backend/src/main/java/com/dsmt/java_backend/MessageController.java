package com.dsmt.java_backend;

import org.springframework.web.bind.annotation.*;

import java.util.List;

@RestController
public class MessageController {

    private final MessageRepository repo;

    public MessageController(MessageRepository repo) {
        this.repo = repo;
    }

    @GetMapping("/api/messages")
    public List<Message> getMessages() {
        return repo.findAll();
    }

    @PostMapping("/api/messages/add")
    public Message addMessage(@RequestParam String text) {
        Message m = new Message(text);
        return repo.save(m);
    }
}
