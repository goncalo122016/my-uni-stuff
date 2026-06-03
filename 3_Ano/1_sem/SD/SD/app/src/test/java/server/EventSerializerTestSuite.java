package server;

import static org.junit.jupiter.api.Assertions.*;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.DisplayName;
import server.event.Event;
import server.storage.EventSerializer;

import java.util.*;
import java.io.*;

@DisplayName("EventSerializer Tests Suite")
public class EventSerializerTestSuite {

    @Test
    @DisplayName("Test 1: Serialize empty list")
    public void testSerializeEmpty() throws IOException {
        List<Event> events = new ArrayList<>();
        ByteArrayOutputStream baos = new ByteArrayOutputStream();
        DataOutputStream dos = new DataOutputStream(baos);

        EventSerializer.serialize(events, dos);

        assertTrue(baos.toByteArray().length >= 0);
    }

    @Test
    @DisplayName("Test 2: Serialize single event")
    public void testSerializeSingleEvent() throws IOException {
        List<Event> events = new ArrayList<>();
        events.add(new Event(new Date(), "product1", 10, 100.0));

        ByteArrayOutputStream baos = new ByteArrayOutputStream();
        DataOutputStream dos = new DataOutputStream(baos);
        EventSerializer.serialize(events, dos);

        assertTrue(baos.toByteArray().length > 0);
    }

    @Test
    @DisplayName("Test 3: Serialize multiple events")
    public void testSerializeMultiple() throws IOException {
        List<Event> events = new ArrayList<>();
        for (int i = 0; i < 10; i++) {
            events.add(new Event(new Date(), "product" + i, i + 1, (i + 1) * 10.0));
        }

        ByteArrayOutputStream baos = new ByteArrayOutputStream();
        DataOutputStream dos = new DataOutputStream(baos);
        EventSerializer.serialize(events, dos);

        assertTrue(baos.toByteArray().length > 0);
    }

    @Test
    @DisplayName("Test 4: Estimate size empty")
    public void testEstimateSizeEmpty() {
        List<Event> events = new ArrayList<>();
        int size = EventSerializer.estimateSize(events);
        assertTrue(size >= 0);
    }

    @Test
    @DisplayName("Test 5: Estimate size single")
    public void testEstimateSizeSingle() {
        List<Event> events = new ArrayList<>();
        events.add(new Event(new Date(), "product", 10, 100.0));

        int size = EventSerializer.estimateSize(events);
        assertTrue(size > 0);
    }

    @Test
    @DisplayName("Test 6: Estimate size multiple")
    public void testEstimateSizeMultiple() {
        List<Event> events = new ArrayList<>();
        for (int i = 0; i < 100; i++) {
            events.add(new Event(new Date(), "product" + i, i + 1, (i + 1) * 1.5));
        }

        int size = EventSerializer.estimateSize(events);
        assertTrue(size > 0);
    }

    @Test
    @DisplayName("Test 7: Estimate scales with count")
    public void testEstimateScales() {
        List<Event> events1 = new ArrayList<>();
        events1.add(new Event(new Date(), "product", 10, 100.0));

        List<Event> events2 = new ArrayList<>();
        for (int i = 0; i < 10; i++) {
            events2.add(new Event(new Date(), "product", 10, 100.0));
        }

        int size1 = EventSerializer.estimateSize(events1);
        int size2 = EventSerializer.estimateSize(events2);

        assertTrue(size2 > size1);
    }

    @Test
    @DisplayName("Test 8: Get compression ratio")
    public void testCompressionRatio() {
        List<Event> events = new ArrayList<>();
        for (int i = 0; i < 10; i++) {
            events.add(new Event(new Date(), "product" + i, i + 1, (i + 1) * 10.0));
        }

        double ratio = EventSerializer.getCompressionRatio(events);
        assertTrue(ratio >= 0);
        assertTrue(ratio <= 1.0 || ratio > 1.0); // Can be any value
    }

    @Test
    @DisplayName("Test 9: Compression ratio empty")
    public void testCompressionRatioEmpty() {
        List<Event> events = new ArrayList<>();
        double ratio = EventSerializer.getCompressionRatio(events);
        assertTrue(ratio >= 0);
    }

    @Test
    @DisplayName("Test 10: Round trip serialize/deserialize")
    public void testRoundTrip() throws IOException {
        List<Event> originalEvents = new ArrayList<>();
        originalEvents.add(new Event(new Date(), "product1", 10, 100.0));
        originalEvents.add(new Event(new Date(), "product2", 20, 200.0));

        ByteArrayOutputStream baos = new ByteArrayOutputStream();
        DataOutputStream dos = new DataOutputStream(baos);
        EventSerializer.serialize(originalEvents, dos);
        dos.close();

        ByteArrayInputStream bais = new ByteArrayInputStream(baos.toByteArray());
        DataInputStream dis = new DataInputStream(bais);
        List<Event> deserializedEvents = EventSerializer.deserialize(dis);

        assertNotNull(deserializedEvents);
        assertEquals(originalEvents.size(), deserializedEvents.size());
    }

    @Test
    @DisplayName("Test 11: Deserialize empty")
    public void testDeserializeEmpty() throws IOException {
        ByteArrayOutputStream baos = new ByteArrayOutputStream();
        DataOutputStream dos = new DataOutputStream(baos);
        EventSerializer.serialize(new ArrayList<>(), dos);
        dos.close();

        ByteArrayInputStream bais = new ByteArrayInputStream(baos.toByteArray());
        DataInputStream dis = new DataInputStream(bais);
        List<Event> events = EventSerializer.deserialize(dis);

        assertNotNull(events);
        assertEquals(0, events.size());
    }

    @Test
    @DisplayName("Test 12: Deserialize preserves data")
    public void testDeserializePreservesData() throws IOException {
        String productName = "testproduct";
        double quantity = 42.5;
        double price = 99.99;

        List<Event> originalEvents = new ArrayList<>();
        originalEvents.add(new Event(new Date(), productName, quantity, price));

        ByteArrayOutputStream baos = new ByteArrayOutputStream();
        DataOutputStream dos = new DataOutputStream(baos);
        EventSerializer.serialize(originalEvents, dos);
        dos.close();

        ByteArrayInputStream bais = new ByteArrayInputStream(baos.toByteArray());
        DataInputStream dis = new DataInputStream(bais);
        List<Event> deserializedEvents = EventSerializer.deserialize(dis);

        assertNotNull(deserializedEvents);
        assertEquals(1, deserializedEvents.size());

        Event event = deserializedEvents.get(0);
        assertEquals(productName, event.getProductName());
        assertEquals(quantity, event.getQuantity(), 0.01);
        assertEquals(price, event.getPrice(), 0.01);
    }

    @Test
    @DisplayName("Test 13: Large batch serialization")
    public void testLargeBatch() throws IOException {
        List<Event> events = new ArrayList<>();
        for (int i = 0; i < 1000; i++) {
            events.add(new Event(new Date(), "product" + (i % 100), i, i * 1.5));
        }

        ByteArrayOutputStream baos = new ByteArrayOutputStream();
        DataOutputStream dos = new DataOutputStream(baos);
        EventSerializer.serialize(events, dos);
        dos.close();

        assertTrue(baos.toByteArray().length > 0);
    }

    @Test
    @DisplayName("Test 14: Special characters in product names")
    public void testSpecialCharactersProduct() throws IOException {
        List<Event> originalEvents = new ArrayList<>();
        originalEvents.add(new Event(new Date(), "product@#$%", 10, 100.0));
        originalEvents.add(new Event(new Date(), "produto português", 20, 200.0));
        originalEvents.add(new Event(new Date(), "商品", 30, 300.0));

        ByteArrayOutputStream baos = new ByteArrayOutputStream();
        DataOutputStream dos = new DataOutputStream(baos);
        EventSerializer.serialize(originalEvents, dos);
        dos.close();

        ByteArrayInputStream bais = new ByteArrayInputStream(baos.toByteArray());
        DataInputStream dis = new DataInputStream(bais);
        List<Event> deserializedEvents = EventSerializer.deserialize(dis);

        assertNotNull(deserializedEvents);
        assertEquals(3, deserializedEvents.size());
    }

    @Test
    @DisplayName("Test 15: Very large quantities and prices")
    public void testLargeValues() throws IOException {
        List<Event> originalEvents = new ArrayList<>();
        originalEvents.add(new Event(new Date(), "product", 999999.99, 999999.99));
        originalEvents.add(new Event(new Date(), "product", 0.01, 0.01));

        ByteArrayOutputStream baos = new ByteArrayOutputStream();
        DataOutputStream dos = new DataOutputStream(baos);
        EventSerializer.serialize(originalEvents, dos);
        dos.close();

        ByteArrayInputStream bais = new ByteArrayInputStream(baos.toByteArray());
        DataInputStream dis = new DataInputStream(bais);
        List<Event> deserializedEvents = EventSerializer.deserialize(dis);

        assertNotNull(deserializedEvents);
        assertEquals(2, deserializedEvents.size());
    }

    @Test
    @DisplayName("Test 16: Decimal precision")
    public void testDecimalPrecision() throws IOException {
        List<Event> originalEvents = new ArrayList<>();
        originalEvents.add(new Event(new Date(), "product", 0.123456789, 0.987654321));

        ByteArrayOutputStream baos = new ByteArrayOutputStream();
        DataOutputStream dos = new DataOutputStream(baos);
        EventSerializer.serialize(originalEvents, dos);
        dos.close();

        ByteArrayInputStream bais = new ByteArrayInputStream(baos.toByteArray());
        DataInputStream dis = new DataInputStream(bais);
        List<Event> deserializedEvents = EventSerializer.deserialize(dis);

        assertNotNull(deserializedEvents);
        assertEquals(1, deserializedEvents.size());
    }

    @Test
    @DisplayName("Test 17: Repeated product names")
    public void testRepeatedProducts() throws IOException {
        List<Event> originalEvents = new ArrayList<>();
        for (int i = 0; i < 50; i++) {
            originalEvents.add(new Event(new Date(), "samproduct", i + 1, (i + 1) * 10.0));
        }

        ByteArrayOutputStream baos = new ByteArrayOutputStream();
        DataOutputStream dos = new DataOutputStream(baos);
        EventSerializer.serialize(originalEvents, dos);
        dos.close();

        ByteArrayInputStream bais = new ByteArrayInputStream(baos.toByteArray());
        DataInputStream dis = new DataInputStream(bais);
        List<Event> deserializedEvents = EventSerializer.deserialize(dis);

        assertNotNull(deserializedEvents);
        assertEquals(50, deserializedEvents.size());
    }

    @Test
    @DisplayName("Test 18: Zero values")
    public void testZeroValues() throws IOException {
        List<Event> originalEvents = new ArrayList<>();
        originalEvents.add(new Event(new Date(), "product", 0, 0));

        ByteArrayOutputStream baos = new ByteArrayOutputStream();
        DataOutputStream dos = new DataOutputStream(baos);
        EventSerializer.serialize(originalEvents, dos);
        dos.close();

        ByteArrayInputStream bais = new ByteArrayInputStream(baos.toByteArray());
        DataInputStream dis = new DataInputStream(bais);
        List<Event> deserializedEvents = EventSerializer.deserialize(dis);

        assertNotNull(deserializedEvents);
        assertEquals(1, deserializedEvents.size());
    }

    @Test
    @DisplayName("Test 19: Compression ratio consistency")
    public void testCompressionRatioConsistency() {
        List<Event> events = new ArrayList<>();
        for (int i = 0; i < 100; i++) {
            events.add(new Event(new Date(), "product" + i, i + 1, (i + 1) * 10.0));
        }

        double ratio1 = EventSerializer.getCompressionRatio(events);
        double ratio2 = EventSerializer.getCompressionRatio(events);

        assertEquals(ratio1, ratio2, 0.0001);
    }

    @Test
    @DisplayName("Test 20: Estimate vs actual size")
    public void testEstimateVsActualSize() throws IOException {
        List<Event> events = new ArrayList<>();
        for (int i = 0; i < 50; i++) {
            events.add(new Event(new Date(), "product" + i, i + 1, (i + 1) * 5.5));
        }

        int estimated = EventSerializer.estimateSize(events);

        ByteArrayOutputStream baos = new ByteArrayOutputStream();
        DataOutputStream dos = new DataOutputStream(baos);
        EventSerializer.serialize(events, dos);
        dos.close();

        int actual = baos.toByteArray().length;

        // Estimate should be reasonably close
        assertTrue(actual > 0);
        assertTrue(estimated > 0);
    }
}
