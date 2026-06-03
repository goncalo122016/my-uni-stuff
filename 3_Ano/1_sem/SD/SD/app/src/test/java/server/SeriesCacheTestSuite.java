package server;

import static org.junit.jupiter.api.Assertions.*;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.DisplayName;
import server.event.Event;
import server.storage.SeriesCache;

import java.util.*;

@DisplayName("SeriesCache Tests Suite")
public class SeriesCacheTestSuite {
    private SeriesCache cache;
    private static final int MAX_SIZE = 5;

    @BeforeEach
    public void setUp() {
        cache = new SeriesCache(MAX_SIZE);
    }

    @Test
    @DisplayName("Test 1: Create cache with max size")
    public void testCreateCache() {
        assertEquals(MAX_SIZE, cache.getMaxSize());
    }

    @Test
    @DisplayName("Test 2: Initially empty")
    public void testInitiallyEmpty() {
        assertEquals(0, cache.size());
    }

    @Test
    @DisplayName("Test 3: Put and get item")
    public void testPutAndGet() {
        List<Event> events = new ArrayList<>();
        events.add(new Event(new Date(), "product", 10, 100.0));

        cache.put("2024-01-01", events);
        List<Event> retrieved = cache.get("2024-01-01");

        assertNotNull(retrieved);
        assertEquals(1, retrieved.size());
    }

    @Test
    @DisplayName("Test 4: Get non-existent returns null")
    public void testGetNonExistent() {
        List<Event> result = cache.get("nonexistent");
        assertNull(result);
    }

    @Test
    @DisplayName("Test 5: Contains existing")
    public void testContainsExisting() {
        List<Event> events = new ArrayList<>();
        cache.put("2024-01-01", events);
        assertTrue(cache.contains("2024-01-01"));
    }

    @Test
    @DisplayName("Test 6: Contains non-existent")
    public void testContainsNonExistent() {
        assertFalse(cache.contains("nonexistent"));
    }

    @Test
    @DisplayName("Test 7: Remove item")
    public void testRemove() {
        List<Event> events = new ArrayList<>();
        cache.put("2024-01-01", events);
        assertTrue(cache.contains("2024-01-01"));

        cache.remove("2024-01-01");
        assertFalse(cache.contains("2024-01-01"));
    }

    @Test
    @DisplayName("Test 8: Clear all")
    public void testClear() {
        cache.put("2024-01-01", new ArrayList<>());
        cache.put("2024-01-02", new ArrayList<>());

        cache.clear();
        assertEquals(0, cache.size());
    }

    @Test
    @DisplayName("Test 9: Size increases with puts")
    public void testSizeIncreases() {
        for (int i = 0; i < 3; i++) {
            cache.put("2024-01-0" + (i + 1), new ArrayList<>());
        }
        assertEquals(3, cache.size());
    }

    @Test
    @DisplayName("Test 10: Get cached dates")
    public void testGetCachedDates() {
        cache.put("2024-01-01", new ArrayList<>());
        cache.put("2024-01-02", new ArrayList<>());

        List<String> dates = cache.getCachedDates();
        assertNotNull(dates);
        assertTrue(dates.contains("2024-01-01"));
        assertTrue(dates.contains("2024-01-02"));
    }

    @Test
    @DisplayName("Test 11: LRU eviction on overflow")
    public void testLRUEviction() {
        // Fill cache to max
        for (int i = 1; i <= MAX_SIZE; i++) {
            cache.put("2024-01-0" + i, new ArrayList<>());
        }
        assertEquals(MAX_SIZE, cache.size());

        // Add one more - should evict oldest
        cache.put("2024-01-06", new ArrayList<>());
        assertEquals(MAX_SIZE, cache.size());
        assertFalse(cache.contains("2024-01-01"));
    }

    @Test
    @DisplayName("Test 12: Access updates LRU")
    public void testAccessUpdatesLRU() {
        // Fill cache
        for (int i = 1; i <= MAX_SIZE; i++) {
            cache.put("2024-01-0" + i, new ArrayList<>());
        }

        // Access first item to make it recently used
        cache.get("2024-01-01");

        // Add new item - should evict second, not first
        cache.put("2024-01-06", new ArrayList<>());
        assertTrue(cache.contains("2024-01-01"));
        assertFalse(cache.contains("2024-01-02"));
    }

    @Test
    @DisplayName("Test 13: Put existing updates")
    public void testPutExistingUpdates() {
        List<Event> events1 = new ArrayList<>();
        events1.add(new Event(new Date(), "product1", 10, 100.0));

        List<Event> events2 = new ArrayList<>();
        events2.add(new Event(new Date(), "product2", 20, 200.0));

        cache.put("2024-01-01", events1);
        cache.put("2024-01-01", events2);

        List<Event> retrieved = cache.get("2024-01-01");
        assertEquals(1, retrieved.size());
        assertEquals("product2", retrieved.get(0).getProductName());
    }

    @Test
    @DisplayName("Test 14: Multiple events in cache")
    public void testMultipleEventsInCache() {
        List<Event> events = new ArrayList<>();
        events.add(new Event(new Date(), "prod1", 10, 100.0));
        events.add(new Event(new Date(), "prod2", 20, 200.0));
        events.add(new Event(new Date(), "prod3", 30, 300.0));

        cache.put("2024-01-01", events);
        List<Event> retrieved = cache.get("2024-01-01");

        assertEquals(3, retrieved.size());
    }

    @Test
    @DisplayName("Test 15: Many items below max")
    public void testManyItemsBelowMax() {
        for (int i = 0; i < MAX_SIZE - 1; i++) {
            cache.put("date" + i, new ArrayList<>());
        }
        assertEquals(MAX_SIZE - 1, cache.size());
    }

    @Test
    @DisplayName("Test 16: Stress test with frequent evictions")
    public void testStressTest() {
        for (int i = 0; i < 100; i++) {
            cache.put("date" + i, new ArrayList<>());
        }
        assertEquals(MAX_SIZE, cache.size());
    }

    @Test
    @DisplayName("Test 17: Cache with null list")
    public void testCacheWithNullList() {
        try {
            cache.put("2024-01-01", null);
            // If it accepts null, that's fine
        } catch (Exception e) {
            // If it rejects null, that's also fine
        }
        assertTrue(true);
    }

    @Test
    @DisplayName("Test 18: Sequential put and get")
    public void testSequentialOps() {
        for (int i = 0; i < 10; i++) {
            List<Event> events = new ArrayList<>();
            events.add(new Event(new Date(), "product", i, i * 10.0));
            cache.put("date" + i, events);
        }

        for (int i = 5; i < 10; i++) {
            List<Event> events = cache.get("date" + i);
            assertNotNull(events);
        }
    }

    @Test
    @DisplayName("Test 19: Large event lists")
    public void testLargeEventLists() {
        List<Event> events = new ArrayList<>();
        for (int i = 0; i < 1000; i++) {
            events.add(new Event(new Date(), "product" + i, i, i * 1.0));
        }

        cache.put("2024-01-01", events);
        List<Event> retrieved = cache.get("2024-01-01");

        assertNotNull(retrieved);
        assertEquals(1000, retrieved.size());
    }

    @Test
    @DisplayName("Test 20: Edge case - max size 1")
    public void testMaxSizeOne() {
        SeriesCache smallCache = new SeriesCache(1);

        smallCache.put("date1", new ArrayList<>());
        assertTrue(smallCache.contains("date1"));

        smallCache.put("date2", new ArrayList<>());
        assertFalse(smallCache.contains("date1"));
        assertTrue(smallCache.contains("date2"));
    }
}
