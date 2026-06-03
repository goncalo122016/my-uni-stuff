import java.util.*;
import java.util.concurrent.locks.*;

public class MuseumManager implements IMuseumManager {
    private final Lock l = new ReentrantLock();
    private final Condition[] canEnter; // uma condição por galeria

    private final int N;
    private final int C;
    private int nextTicketId = 0;

    private final Map<Integer, Integer> galleries;          // pessoas dentro
    private final Map<Integer, Integer> waitingPerGallery; // pessoas à espera
    private final Map<String, Integer> tickets;             // usos restantes
    private final Map<String, Set<Integer>> ticketHistory;  // galerias visitadas

    public MuseumManager(int N, int C) {
        this.N = N;
        this.C = C;

        galleries = new HashMap<>();
        waitingPerGallery = new HashMap<>();
        tickets = new HashMap<>();
        ticketHistory = new HashMap<>();

        canEnter = new Condition[N + 1];

        for (int i = 1; i <= N; i++) {
            galleries.put(i, 0);
            waitingPerGallery.put(i, 0);
            canEnter[i] = l.newCondition();
        }
    }

    @Override
    public String buyTicket(int uses) {
        l.lock();
        try {
            String id = String.valueOf(nextTicketId++);
            tickets.put(id, uses);
            ticketHistory.put(id, new HashSet<>());
            return id;
        } finally {
            l.unlock();
        }
    }

    @Override
    public int enterGallery(int galleryId, String ticketId) throws InterruptedException {
        l.lock();
        try {
            Integer uses = tickets.get(ticketId);
            if (uses == null || uses <= 0)
                return 0;

            if (galleryId == 0) {
                while (true) {
                    for (int g = 1; g <= N; g++) {
                        if (!ticketHistory.get(ticketId).contains(g)
                                && galleries.get(g) < C) {
                            galleryId = g;
                            break;
                        }
                    }
                    if (galleryId != 0)
                        break;

                    // nenhuma galeria disponível -> esperar numa qualquer
                    canEnter[1].await();
                }
            }

            waitingPerGallery.put(galleryId, waitingPerGallery.get(galleryId) + 1);

            while (galleries.get(galleryId) >= C) {
                canEnter[galleryId].await();
            }

            waitingPerGallery.put(galleryId, waitingPerGallery.get(galleryId) - 1);

            galleries.put(galleryId, galleries.get(galleryId) + 1);
            tickets.put(ticketId, uses - 1);
            ticketHistory.get(ticketId).add(galleryId);

            return galleryId;

        } finally {
            l.unlock();
        }
    }

    @Override
    public void exitGallery(int galleryId, String ticketId) {
        l.lock();
        try {
            galleries.put(galleryId, galleries.get(galleryId) - 1);
            canEnter[galleryId].signal();
        } finally {
            l.unlock();
        }
    }

    @Override
    public Map<Integer, Integer> peopleWaitingPerGallery() {
        l.lock();
        try {
            return new HashMap<>(waitingPerGallery);
        } finally {
            l.unlock();
        }
    }
}
