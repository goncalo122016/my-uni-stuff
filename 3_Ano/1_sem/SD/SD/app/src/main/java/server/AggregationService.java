package server;

import server.event.Event;
import server.event.EventService;

import java.util.*;
import java.util.concurrent.locks.ReentrantLock;

public class AggregationService {
    private final EventService eventService;
    private final int D;

    private final Map<String, AggregationResult> cache = new HashMap<>();
    private final ReentrantLock cacheLock = new ReentrantLock();

    public static class AggregationResult {
        public double quantity;
        public double volume;
        public double averagePrice;
        public double maxPrice;

        public AggregationResult() {
            this.quantity = 0;
            this.volume = 0;
            this.averagePrice = 0;
            this.maxPrice = 0;
        }

        @Override
        public String toString() {
            return String.format("Qty: %.2f | Vol: €%.2f | Avg: €%.2f | Max: €%.2f",
                    quantity, volume, averagePrice, maxPrice);
        }
    }

    public AggregationService(EventService eventService, int D) {
        this.eventService = eventService;
        this.D = D;
    }

    public double getQuantitySold(String productName, int days) {
        if (days < 1 || days > D) {
            throw new IllegalArgumentException("Dias deve estar entre 1 e " + D);
        }

        AggregationResult result = getOrComputeAggregation(productName, days);
        return result.quantity;
    }

    public double getSalesVolume(String productName, int days) {
        if (days < 1 || days > D) {
            throw new IllegalArgumentException("Dias deve estar entre 1 e " + D);
        }

        AggregationResult result = getOrComputeAggregation(productName, days);
        return result.volume;
    }

    public double getAveragePrice(String productName, int days) {
        if (days < 1 || days > D) {
            throw new IllegalArgumentException("Dias deve estar entre 1 e " + D);
        }

        AggregationResult result = getOrComputeAggregation(productName, days);
        return result.averagePrice;
    }

    public double getMaxPrice(String productName, int days) {
        if (days < 1 || days > D) {
            throw new IllegalArgumentException("Dias deve estar entre 1 e " + D);
        }

        AggregationResult result = getOrComputeAggregation(productName, days);
        return result.maxPrice;
    }

    private AggregationResult getOrComputeAggregation(String productName, int days) {
        String cacheKey = productName + "|" + days;

        cacheLock.lock();
        try {
            // Verificar cache
            if (cache.containsKey(cacheKey)) {
                System.out.println("[AGGREGATION] Cache hit para " + productName + " (últimos " + days + " dias)");
                return cache.get(cacheKey);
            }

            // Não está em cache - computar on-demand
            System.out.println(
                    "[AGGREGATION] Cache miss para " + productName + " (últimos " + days + " dias) - computando...");
            AggregationResult result = computeAggregation(productName, days);

            // Guardar no cache
            cache.put(cacheKey, result);
            System.out.println("[AGGREGATION] Resultado cacheado para " + productName);

            return result;
        } finally {
            cacheLock.unlock();
        }
    }

    private AggregationResult computeAggregation(String productName, int days) {
        AggregationResult result = new AggregationResult();
        String todayStr = eventService.getCurrentSimulatedDate();

        for (int i = 1; i <= days; i++) {
            String dayStr = subtractDays(todayStr, i);

            try {
                eventService.getSeriesCache().markProcessing(dayStr, true);

                List<Event> events = eventService.getEventsByDate(dayStr);

                if (events == null || events.isEmpty()) {
                    continue;
                }

                for (Event event : events) {
                    if (event.getProductName().equalsIgnoreCase(productName)) {
                        double qty = event.getQuantity();
                        double price = event.getPrice();

                        result.quantity += qty;
                        result.volume += qty * price;

                        if (price > result.maxPrice) {
                            result.maxPrice = price;
                        }
                    }
                }

            } catch (Exception e) {
                System.err.println("[AGGREGATION] Erro no dia " + dayStr + ": " + e.getMessage());
            } finally {
                eventService.getSeriesCache().markProcessing(dayStr, false);
            }
        }

        if (result.quantity > 0)

        {
            result.averagePrice = result.volume / result.quantity;
        }

        return result;
    }

    private String subtractDays(String date, int days) {
        try {
            Calendar cal = Calendar.getInstance();
            cal.setTime(eventService.getDateFormat().parse(date));
            cal.add(Calendar.DAY_OF_MONTH, -days);
            return eventService.getDateFormat().format(cal.getTime());
        } catch (Exception e) {
            return date;
        }
    }

    public void invalidateCache() {
        cacheLock.lock();
        try {
            cache.clear();
            System.out.println("[AGGREGATION] Cache invalidado (dia avançado/reset)");
        } finally {
            cacheLock.unlock();
        }
    }

    public String getCacheStats() {
        cacheLock.lock();
        try {
            return String.format("Entradas em cache: %d | D (dias): %d", cache.size(), D);
        } finally {
            cacheLock.unlock();
        }
    }
}
