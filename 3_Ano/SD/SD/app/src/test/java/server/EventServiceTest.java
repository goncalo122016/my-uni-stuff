package server;

import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import server.event.Event;
import server.event.EventService;
import server.storage.EventSerializer;
import server.storage.SeriesCache;

import java.io.ByteArrayInputStream;
import java.io.ByteArrayOutputStream;
import java.io.DataInputStream;
import java.io.DataOutputStream;
import java.util.*;

import static org.junit.jupiter.api.Assertions.*;

public class EventServiceTest {

    private EventService eventService;

    @BeforeEach
    public void setUp() {
        eventService = new EventService(5,10); // Cache de 5 séries
    }

    @Test
    public void testAddEvent() {
        assertTrue(eventService.addEvent("Apple", 10, 1.5));
        assertEquals(1, eventService.getTodayEvents().size());

        assertTrue(eventService.addEvent("Banana", 5, 2.0));
        assertEquals(2, eventService.getTodayEvents().size());
    }

    @Test
    public void testAddEventInvalidData() {
        assertFalse(eventService.addEvent(null, 10, 1.5));
        assertFalse(eventService.addEvent("", 10, 1.5));
        assertFalse(eventService.addEvent("Apple", -1, 1.5));
        assertFalse(eventService.addEvent("Apple", 10, -1));
    }

    @Test
    public void testGetTodayTotal() {
        eventService.addEvent("Apple", 10, 1.5); // 15
        eventService.addEvent("Banana", 5, 2.0); // 10
        assertEquals(25.0, eventService.getTodayTotal(), 0.01);
    }

    @Test
    public void testAdvanceDay() {
        eventService.addEvent("Apple", 10, 1.5);
        String dateToday = eventService.getCurrentSimulatedDate();

        eventService.advanceDay();
        String dateTomorrow = eventService.getCurrentSimulatedDate();

        assertNotEquals(dateToday, dateTomorrow);
        assertEquals(0, eventService.getTodayEvents().size()); // Eventos anteriores apagados
    }

    @Test
    public void testNotificationServiceSimultaneous() throws InterruptedException {
        NotificationService notif = eventService.getNotificationService();

        // Thread que adiciona eventos após delay
        Thread adder = new Thread(() -> {
            try {
                Thread.sleep(100);
                eventService.addEvent("Product1", 1, 1.0);
                Thread.sleep(100);
                eventService.addEvent("Product2", 1, 1.0);
            } catch (InterruptedException e) {
                e.printStackTrace();
            }
        });

        adder.start();

        // Aguardar vendas simultâneas (com timeout de 2 segundos)
        boolean result = notif.waitForSimultaneousSales("Product1", "Product2", 2000);
        assertTrue(result, "Deveria ter encontrado vendas simultâneas");

        adder.join();
    }

    @Test
    public void testNotificationServiceConsecutive() throws InterruptedException {
        NotificationService notif = eventService.getNotificationService();

        // Thread que adiciona eventos consecutivos
        Thread adder = new Thread(() -> {
            try {
                for (int i = 0; i < 3; i++) {
                    eventService.addEvent("Banana", 1, 1.0);
                    Thread.sleep(50);
                }
            } catch (InterruptedException e) {
                e.printStackTrace();
            }
        });

        adder.start();

        // Aguardar 3 vendas consecutivas
        String product = notif.waitForConsecutiveSales(3, 2000);
        assertEquals("Banana", product, "Deveria ter encontrado 3 vendas consecutivas");

        adder.join();
    }

    @Test
    public void testNotificationServiceTimeout() throws InterruptedException {
        NotificationService notif = eventService.getNotificationService();

        // Não adiciona eventos - deve dar timeout
        long start = System.currentTimeMillis();
        boolean result = notif.waitForSimultaneousSales("Product1", "Product2", 500);
        long duration = System.currentTimeMillis() - start;

        assertFalse(result, "Deveria ter expirado");
        assertTrue(duration >= 500, "Deveria ter esperado pelo menos 500ms");
    }

    @Test
    public void testSeriesCache() {
        SeriesCache cache = eventService.getSeriesCache();

        List<Event> events1 = Arrays.asList(
                new Event(new Date(), "Product1", 1, 1.0),
                new Event(new Date(), "Product2", 1, 1.0));

        // Adicionar ao cache
        cache.put("2024-01-01", events1);
        assertTrue(cache.contains("2024-01-01"));

        // Recuperar do cache
        List<Event> retrieved = cache.get("2024-01-01");
        assertEquals(2, retrieved.size());
        assertEquals("Product1", retrieved.get(0).getProductName());
    }

    @Test
    public void testSeriesCacheLRU() {
        SeriesCache cache = eventService.getSeriesCache();

        // Adicionar 6 séries ao cache (limite é 5)
        for (int i = 0; i < 6; i++) {
            List<Event> events = Arrays.asList(
                    new Event(new Date(), "Product" + i, 1, 1.0));
            cache.put("2024-01-0" + (i + 1), events);
        }

        // Cache deve ter apenas 5 itens
        assertEquals(5, cache.size(), "Cache deve conter máximo 5 itens");

        // O primeiro deve ter sido removido (LRU)
        assertFalse(cache.contains("2024-01-01"));
    }

    @Test
    public void testEventSerializer() throws Exception {
        List<Event> events = Arrays.asList(
                new Event(new Date(), "Apple", 10, 1.5),
                new Event(new Date(), "Banana", 5, 2.0),
                new Event(new Date(), "Apple", 3, 1.5));

        // Serializar
        ByteArrayOutputStream baos = new ByteArrayOutputStream();
        DataOutputStream dos = new DataOutputStream(baos);
        EventSerializer.serialize(events, dos);

        // Desserializar
        ByteArrayInputStream bais = new ByteArrayInputStream(baos.toByteArray());
        DataInputStream dis = new DataInputStream(bais);
        List<Event> recovered = EventSerializer.deserialize(dis);

        assertEquals(3, recovered.size());
        assertEquals("Apple", recovered.get(0).getProductName());
        assertEquals(10, recovered.get(0).getQuantity(), 0.01);
    }

    @Test
    public void testEventSerializerCompression() {
        List<Event> events = new ArrayList<>();
        for (int i = 0; i < 100; i++) {
            events.add(new Event(new Date(), "Apple", 1, 1.0));
        }

        double ratio = EventSerializer.getCompressionRatio(events);
        assertTrue(ratio < 1.0, "Compressão deveria estar ativa");
        System.out.println("Taxa de compressão: " + (ratio * 100) + "%");
    }

    @Test
    public void testMultipleClients() throws InterruptedException {
        // Simular múltiplos clientes
        Thread client1 = new Thread(() -> {
            for (int i = 0; i < 10; i++) {
                eventService.addEvent("Client1_Product", 1, 1.0);
            }
        });

        Thread client2 = new Thread(() -> {
            for (int i = 0; i < 10; i++) {
                eventService.addEvent("Client2_Product", 1, 1.0);
            }
        });

        client1.start();
        client2.start();
        client1.join();
        client2.join();

        // Deve ter 20 eventos no total
        assertEquals(20, eventService.getTodayEvents().size());
    }
}
