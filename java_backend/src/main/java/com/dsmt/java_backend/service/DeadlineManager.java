package com.dsmt.java_backend.service;
import com.dsmt.java_backend.model.Event;
import com.dsmt.java_backend.repository.EventRepository;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.scheduling.TaskScheduler;
import org.springframework.scheduling.annotation.Scheduled;
import org.springframework.stereotype.Service;

import java.time.*;
import java.util.Date;
import java.util.List;
@Service
public class DeadlineManager {

    @Autowired
    private EventRepository eventRepository;

    @Autowired
    private ErlangService erlangService;

    @Autowired
    private TaskScheduler taskScheduler;

    // Ogni giorno alle 00:01 analizza le deadline odierne
    @Scheduled(cron = "0 0 * * * *")
    public void scheduleTodaysDeadlines() {
        System.out.println("Todays deadlines");

        LocalDateTime startOfDay = LocalDate.now().atStartOfDay();
        LocalDateTime endOfDay = LocalDate.now().atTime(LocalTime.MAX);
        LocalDateTime now = LocalDateTime.now();
        System.out.println(">> Orario attuale: " + now);


        List<Event> todayEvents = eventRepository.findByDeadlineBetween(startOfDay, endOfDay);

        System.out.println(">> Eventi trovati nel DB per oggi: " + todayEvents.size());

        for (Event event : todayEvents) {

            if (!event.getDeadline().isAfter(now)) {
                continue;
            }
            long delayInMillis = Duration.between(now, event.getDeadline()).toMillis();

            System.out.println(">> Scheduling evento ID " + event.getId() + " per le " + event.getDeadline());
            erlangService.sendTimerRequest(event.getId(), delayInMillis);
            //taskScheduler.schedule(() -> {erlangService.triggerGlobalOptimum(event.getId());},Date.from(event.getDeadline().atZone(ZoneId.systemDefault()).toInstant()));
        }
    }
}
