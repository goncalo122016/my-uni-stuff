package server.storage;

import server.event.Event;

import java.io.*;
import java.util.*;

/**
 * Serializador eficiente para eventos com compressão de nomes de produtos.
 * Utiliza um dicionário para deduplica nomes repetidos e economizar espaço.
 */
public class EventSerializer {

    /**
     * Serializa eventos para output stream com compressão de nomes.
     * Formato:
     * [dict_size] [dict_entry1_len][dict_entry1] ... [events_count]
     * [events_data...]
     */
    public static void serialize(List<Event> events, DataOutputStream dos) throws IOException {
        if (events == null || events.isEmpty()) {
            dos.writeInt(0); // sem eventos
            return;
        }

        // Construir dicionário de produtos únicos
        LinkedHashMap<String, Integer> productDict = new LinkedHashMap<>();
        int dictIndex = 0;
        for (Event e : events) {
            if (!productDict.containsKey(e.getProductName())) {
                productDict.put(e.getProductName(), dictIndex++);
            }
        }

        // Escrever dicionário
        dos.writeInt(productDict.size());
        for (String product : productDict.keySet()) {
            dos.writeUTF(product);
        }

        // Escrever eventos (usando índices do dicionário)
        dos.writeInt(events.size());
        for (Event e : events) {
            int productIndex = productDict.get(e.getProductName());
            dos.writeInt(productIndex);
            dos.writeDouble(e.getQuantity());
            dos.writeDouble(e.getPrice());
        }
    }

    /**
     * Desserializa eventos de input stream.
     */
    public static List<Event> deserialize(DataInputStream dis) throws IOException {
        List<Event> events = new ArrayList<>();

        // Ler dicionário
        int dictSize = dis.readInt();
        if (dictSize == 0) {
            return events; // sem eventos
        }

        Map<Integer, String> dictMap = new HashMap<>();
        for (int i = 0; i < dictSize; i++) {
            String product = dis.readUTF();
            dictMap.put(i, product);
        }

        // Ler eventos
        int eventCount = dis.readInt();
        for (int i = 0; i < eventCount; i++) {
            int productIndex = dis.readInt();
            double quantity = dis.readDouble();
            double price = dis.readDouble();

            String productName = dictMap.get(productIndex);
            if (productName != null) {
                events.add(new Event(new java.util.Date(), productName, quantity, price));
            }
        }

        return events;
    }

    /**
     * Calcula tamanho estimado da serialização (em bytes).
     */
    public static int estimateSize(List<Event> events) {
        if (events == null || events.isEmpty()) {
            return 4; // só o tamanho do dicionário
        }

        Set<String> uniqueProducts = new HashSet<>();
        int totalSize = 4; // dictSize

        for (Event e : events) {
            String product = e.getProductName();
            if (!uniqueProducts.contains(product)) {
                uniqueProducts.add(product);
                totalSize += 2 + product.length(); // UTF string format
            }
        }

        // Events
        totalSize += 4; // eventCount
        totalSize += events.size() * (4 + 8 + 8); // index + quantity + price

        return totalSize;
    }

    /**
     * Obtém a taxa de compressão (original vs comprimido).
     */
    public static double getCompressionRatio(List<Event> events) {
        if (events == null || events.isEmpty()) {
            return 1.0;
        }

        // Tamanho sem dicionário (cada evento com nome completo)
        int uncompressed = 4; // count
        for (Event e : events) {
            uncompressed += 2 + e.getProductName().length() + 8 + 8;
        }

        int compressed = estimateSize(events);
        return (double) compressed / uncompressed;
    }
}
