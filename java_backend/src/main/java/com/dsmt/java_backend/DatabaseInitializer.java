package com.dsmt.java_backend;

import org.springframework.boot.CommandLineRunner;
import org.springframework.stereotype.Component;

@Component
public class DatabaseInitializer implements CommandLineRunner {

    private final MessageRepository repo;

    public DatabaseInitializer(MessageRepository repo) {
        this.repo = repo;
    }

    @Override
    public void run(String... args) {

        // Only insert if table is empty
        if (repo.count() == 0) {
            repo.save(new Message("Hello from Java + MySQL!"));
            repo.save(new Message("Automatic startup data inserted."));
        }
    }
}
