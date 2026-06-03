package server.storage;

import server.event.Event;

import java.util.*;
import java.util.concurrent.locks.ReentrantLock;

/**
 * Cache LRU (Least Recently Used) que mantém no máximo S séries em memória.
 * Quando atinge o limite, remove a menos recentemente usada.
 */
public class SeriesCache {
    private final int maxSize;
    private final LinkedHashMap<String, List<Event>> cache;
    private final Set<String> processingDates;
    private final ReentrantLock lock = new ReentrantLock();

    public SeriesCache(int maxSize) {
        this.maxSize = maxSize;
        this.cache = new LinkedHashMap<>(16, 0.75f, true);
        this.processingDates = new HashSet<>();
    }

    /**
     * Coloca uma série no cache.
     */
    public void put(String date, List<Event> events) {
        lock.lock();
        try {
            if (cache.size() >= maxSize && !cache.containsKey(date)) {
                evictLRU();
            }

            cache.put(date, new ArrayList<>(events));

        } finally {
            lock.unlock();
        }
    }

    /**
     * Obtém uma série do cache (marca como acedida).
     */
    public List<Event> get(String date) {
        lock.lock();
        try {
            List<Event> events = cache.get(date);
            if (events != null) {
                return new ArrayList<>(events);
            }
            return null;
        } finally {
            lock.unlock();
        }
    }

    public void markProcessing(String date, boolean processing) {
        lock.lock();
        try {
            if (processing) {
                processingDates.add(date);
            } else {
                processingDates.remove(date);

                if (cache.size() > maxSize) {
                    evictLRU();
                }
            }
        } finally {
            lock.unlock();
        }
    }

    private boolean evictLRU() {
        Iterator<Map.Entry<String, List<Event>>> it = cache.entrySet().iterator();

        while (it.hasNext()) {
            Map.Entry<String, List<Event>> entry = it.next();
            String date = entry.getKey();

            // Não evict se está sendo processada
            if (!processingDates.contains(date)) {
                it.remove();
                System.out.println("[CACHE] Evicted LRU: " + date);
                return true;
            }
        }

        System.out.println("[CACHE] Não foi possível evict (todas em processamento)");
        return false;
    }

    /**
     * Verifica se uma série está em cache.
     */
    public boolean contains(String date) {
        lock.lock();
        try {
            return cache.containsKey(date);
        } finally {
            lock.unlock();
        }
    }

    /**
     * Remove uma série do cache.
     */
    public void remove(String date) {
        lock.lock();
        try {
            cache.remove(date);
        } finally {
            lock.unlock();
        }
    }

    /**
     * Limpa todo o cache.
     */
    public void clear() {
        lock.lock();
        try {
            cache.clear();
        } finally {
            lock.unlock();
        }
    }

    /**
     * Obtem o tamanho atual do cache.
     */
    public int size() {
        lock.lock();
        try {
            return cache.size();
        } finally {
            lock.unlock();
        }
    }

    /**
     * Obtem o tamanho máximo do cache.
     */
    public int getMaxSize() {
        return maxSize;
    }

    /**
     * Lista as datas em cache (em ordem de acesso).
     */
    public List<String> getCachedDates() {
        lock.lock();
        try {
            return new ArrayList<>(cache.keySet());
        } finally {
            lock.unlock();
        }
    }
}
