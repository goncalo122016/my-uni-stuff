package server.event;

import server.NotificationService;
import server.storage.FileSeriesStore;
import server.storage.SeriesCache;

import java.io.IOException;
import java.text.SimpleDateFormat;
import java.util.*;
import java.util.concurrent.locks.ReentrantLock;

public class EventService {
    private final Map<String, List<Event>> dailyTimeline; // key: date (YYYY-MM-DD), value: list of events
    private final ReentrantLock lock = new ReentrantLock();
    private static final SimpleDateFormat dateFormat = new SimpleDateFormat("yyyy-MM-dd");
    private final NotificationService notificationService;
    private final FileSeriesStore fileStore;
    private final int D;
    private final SeriesCache seriesCache;
    private final int maxCachedSeries; // Parâmetro S

    private long simulatedDayOffset = 0;

    public EventService(int maxCachedSeries, int D) {
        this.dailyTimeline = new HashMap<>();
        this.notificationService = new NotificationService();
        this.maxCachedSeries = maxCachedSeries;
        this.D = D;
        this.seriesCache = new SeriesCache(maxCachedSeries);
        try {
            this.fileStore = new FileSeriesStore("./data");
        } catch (IOException e) {
            throw new RuntimeException("Erro ao inicializar FileSeriesStore", e);
        }
    }

    public EventService() {
        this(5, 10);
    }

    public boolean addEvent(String product, double quantity, double price) {
        if (product == null || product.trim().isEmpty()) {
            System.out.println("[EventService] Produto inválido: " + product);
            return false;
        }

        if (quantity < 0) {
            System.out.println("[EventService] Quantidade inválida: " + quantity);
            return false;
        }

        if (price < 0) {
            System.out.println("[EventService] Preço inválido: " + price);
            return false;
        }

        lock.lock();
        try {
            String currentDate = getCurrentSimulatedDate();

            // System.out.println("[EventService] addEvent: date=" + currentDate + ",
            // product=" + product);

            Event event = new Event(new Date(), product, quantity, price);

            if (!dailyTimeline.containsKey(currentDate)) {
                dailyTimeline.put(currentDate, new ArrayList<>());
                // System.out.println("[EventService] Criada nova lista para data: " +
                // currentDate);
            }

            dailyTimeline.get(currentDate).add(event);

            // System.out.println("[EventService] Total eventos em " + currentDate + ": " +
            // dailyTimeline.get(currentDate).size());

            notificationService.recordEvent(product);

            return true;

        } finally {
            lock.unlock();
        }
    }

    public List<Event> getTodayEvents() {
        lock.lock();
        try {
            String today = getTodayDateStr();
            return new ArrayList<>(dailyTimeline.getOrDefault(today, new ArrayList<>()));
        } finally {
            lock.unlock();
        }
    }

    public List<Event> getEventsByDate(String date) {
        String currentDate = getCurrentSimulatedDate();

        if (date.equals(currentDate)) {
            return getTodayEvents();
        }

        List<Event> cached = seriesCache.get(date);
        if (cached != null) {
            return cached;
        }

        lock.lock();
        try {
            List<Event> events = fileStore.retrieveDay(date);

            if (events != null && !events.isEmpty()) {
                seriesCache.put(date, events);
            }

            return events != null ? events : new ArrayList<>();

        } finally {
            lock.unlock();
        }
    }

    public Map<String, List<Event>> getAllEvents() {
        lock.lock();
        try {

            if (dailyTimeline.isEmpty()) {
                System.out.println("[EventService] ⚠️ dailyTimeline está VAZIO!");
                return new HashMap<>();
            }

            // Mostra detalhes de cada data
            for (Map.Entry<String, List<Event>> entry : dailyTimeline.entrySet()) {
                List<Event> events = entry.getValue();

                // Mostra os primeiros 3 eventos de cada dia (para não poluir muito)
                int count = 0;
                for (Event e : events) {
                    if (count < 3) {
                        System.out.println("[EventService]     - " + e.getProductName() +
                                ", qty=" + e.getQuantity() +
                                ", price=" + e.getPrice());
                        count++;
                    } else if (count == 3) {
                        System.out.println("[EventService]     ... (mais " + (events.size() - 3) + " eventos)");
                        break;
                    }
                }
            }

            Map<String, List<Event>> copy = new HashMap<>(dailyTimeline);
            System.out.println("[EventService] Retornando cópia com " + copy.size() + " datas");

            return copy;

        } finally {
            lock.unlock();
        }
    }

    public double getTodayTotal() {
        lock.lock();
        try {
            String today = getTodayDateStr();
            List<Event> events = dailyTimeline.getOrDefault(today, new ArrayList<>());
            return events.stream()
                    .mapToDouble(e -> e.getQuantity() * e.getPrice())
                    .sum();
        } finally {
            lock.unlock();
        }
    }

    private String getTodayDateStr() {
        Calendar cal = Calendar.getInstance();
        cal.add(Calendar.DAY_OF_YEAR, (int) simulatedDayOffset);
        return dateFormat.format(cal.getTime());
    }

    public void advanceDay() {
        lock.lock();
        try {
            String oldDate = getTodayDateStr();

            notificationService.endDay();

            List<Event> events = dailyTimeline.get(oldDate);
            if (events != null && !events.isEmpty()) {
                try {
                    fileStore.persistDay(oldDate, events);
                    System.out.println("[PERSIST] Dia " + oldDate + " persistido (" +
                            events.size() + " eventos)");
                    dailyTimeline.remove(oldDate);
                } catch (Exception e) {
                    System.err.println("[ERROR] Persistência falhou: " + e.getMessage());
                }
            }

            cleanupOldDays();

            simulatedDayOffset++;

            String newDate = getTodayDateStr();

            notificationService.startNewDay();

            System.out.println("[SERVER] Dia avançado: " + oldDate + " → " + newDate);
            System.out.println("[SERVER] Todos os eventos do dia anterior foram arquivados.");

        } finally {
            lock.unlock();
        }
    }

    public void resetToToday() {
        lock.lock();
        try {
            String oldDate = getTodayDateStr();

            // 1. Termina o dia atual (notifica threads aguardando)
            notificationService.endDay();

            // 2. Reseta offset para hoje
            simulatedDayOffset = 0;

            String newDate = getTodayDateStr();

            // 3. Inicia novo dia
            notificationService.startNewDay();

            System.out.println("[SERVER] Simulação resetada: " + oldDate + " → " + newDate);

        } finally {
            lock.unlock();
        }
    }

    public String getCurrentSimulatedDate() {
        lock.lock();
        try {
            return getTodayDateStr();
        } finally {
            lock.unlock();
        }
    }

    public long getDayOffset() {
        lock.lock();
        try {
            return simulatedDayOffset;
        } finally {
            lock.unlock();
        }
    }

    public NotificationService getNotificationService() {
        return notificationService;
    }

    public FileSeriesStore getFileStore() {
        return fileStore;
    }

    public SeriesCache getSeriesCache() {
        return seriesCache;
    }

    public int getMaxCachedSeries() {
        return maxCachedSeries;
    }

    public void persistPreviousDay() {
        lock.lock();
        try {
            Calendar cal = Calendar.getInstance();
            cal.add(Calendar.DAY_OF_YEAR, (int) simulatedDayOffset - 1);
            String previousDate = dateFormat.format(cal.getTime());

            List<Event> previousEvents = dailyTimeline.get(previousDate);
            if (previousEvents != null && !previousEvents.isEmpty()) {
                fileStore.persistDay(previousDate, previousEvents);
                System.out.println("[SERVER] Dia " + previousDate + " persistido em disco.");
                dailyTimeline.remove(previousDate);
            }
        } finally {
            lock.unlock();
        }
    }

    private void cleanupOldDays() {
        // Calcular data de corte (hoje - D dias)
        Calendar cal = Calendar.getInstance();
        cal.add(Calendar.DAY_OF_YEAR, (int) simulatedDayOffset - D);
        String cutoffDate = dateFormat.format(cal.getTime());

        try {
            // Se esse dia existe no disco, eliminar
            if (fileStore.dayExists(cutoffDate)) {
                fileStore.deleteDay(cutoffDate);
                seriesCache.remove(cutoffDate);
                System.out.println("[CLEANUP] Dia " + cutoffDate + " eliminado (> D dias)");
            }
        } catch (Exception e) {
            System.err.println("[ERROR] Erro ao limpar: " + e.getMessage());
        }
    }

    public SimpleDateFormat getDateFormat() {
        return dateFormat;
    }
}
