package server.storage;

import server.event.Event;

import java.io.*;
import java.nio.file.*;
import java.util.*;
import java.util.concurrent.locks.ReentrantLock;

/**
 * Armazena séries de eventos em disco para persistência e recuperação.
 * Usa serialização binária com DataInputStream/DataOutputStream.
 */
public class FileSeriesStore {
    private final Path storePath;
    private final ReentrantLock lock = new ReentrantLock();

    public FileSeriesStore(String basePath) throws IOException {
        this.storePath = Paths.get(basePath, "events");
        if (!Files.exists(storePath)) {
            Files.createDirectories(storePath);
        }
    }

    /**
     * Persiste uma série de eventos para um dia específico.
     * Formato binário: [count] [product_len][product] [qty] [price] ...
     */
    public boolean persistDay(String date, List<Event> events) {
        lock.lock();
        try {
            Path dayFile = storePath.resolve(date + ".events");
            try (DataOutputStream dos = new DataOutputStream(Files.newOutputStream(dayFile))) {
                dos.writeInt(events.size()); // Número de eventos
                for (Event e : events) {
                    // Escrever nome do produto (string)
                    String productName = e.getProductName();
                    dos.writeUTF(productName);
                    // Escrever quantidade e preço
                    dos.writeDouble(e.getQuantity());
                    dos.writeDouble(e.getPrice());
                }
                return true;
            }
        } catch (IOException e) {
            System.err.println("[FileSeriesStore] Erro ao persistir dia " + date + ": " + e.getMessage());
            return false;
        } finally {
            lock.unlock();
        }
    }

    /**
     * Recupera uma série de eventos de um dia do disco.
     */
    public List<Event> retrieveDay(String date) {
        lock.lock();
        try {
            Path dayFile = storePath.resolve(date + ".events");
            if (!Files.exists(dayFile)) {
                return new ArrayList<>();
            }

            List<Event> events = new ArrayList<>();
            try (DataInputStream dis = new DataInputStream(Files.newInputStream(dayFile))) {
                int count = dis.readInt();
                for (int i = 0; i < count; i++) {
                    String productName = dis.readUTF();
                    double quantity = dis.readDouble();
                    double price = dis.readDouble();
                    events.add(new Event(new Date(), productName, quantity, price));
                }
                return events;
            }
        } catch (IOException e) {
            System.err.println("[FileSeriesStore] Erro ao recuperar dia " + date + ": " + e.getMessage());
            return new ArrayList<>();
        } finally {
            lock.unlock();
        }
    }

    // Streaming: processar eventos em chunks sem carregar tudo
    public void streamDay(String date, EventProcessor processor) throws IOException {
        lock.lock();
        try {
            Path dayFile = storePath.resolve(date + ".events");
            if (!Files.exists(dayFile)) {
                return;
            }

            try (DataInputStream dis = new DataInputStream(
                    new BufferedInputStream(Files.newInputStream(dayFile)))) {

                int totalCount = dis.readInt();
                int chunkSize = 10000; // Processar 10K eventos por vez
                List<Event> chunk = new ArrayList<>(chunkSize);

                for (int i = 0; i < totalCount; i++) {
                    String productName = dis.readUTF();
                    double quantity = dis.readDouble();
                    double price = dis.readDouble();

                    chunk.add(new Event(new Date(), productName, quantity, price));

                    // Quando chunk está cheio, processar e limpar
                    if (chunk.size() >= chunkSize) {
                        processor.process(chunk);
                        chunk.clear();
                    }
                }

                // Processar últimos eventos
                if (!chunk.isEmpty()) {
                    processor.process(chunk);
                }

                System.out.println("[DISK] Streaming completo: " + date +
                        " (" + totalCount + " eventos processados)");
            }
        } finally {
            lock.unlock();
        }
    }

    /**
     * Lista todos os dias armazenados.
     */
    public List<String> listDays() {
        lock.lock();
        try {
            List<String> days = new ArrayList<>();
            try (DirectoryStream<Path> stream = Files.newDirectoryStream(storePath, "*.events")) {
                for (Path path : stream) {
                    String filename = path.getFileName().toString();
                    String date = filename.replace(".events", "");
                    days.add(date);
                }
            } catch (IOException e) {
                System.err.println("[FileSeriesStore] Erro ao listar dias: " + e.getMessage());
            }
            Collections.sort(days);
            return days;
        } finally {
            lock.unlock();
        }
    }

    /**
     * Verifica se um dia existe em disco.
     */
    public boolean dayExists(String date) {
        lock.lock();
        try {
            Path dayFile = storePath.resolve(date + ".events");
            return Files.exists(dayFile);
        } finally {
            lock.unlock();
        }
    }

    /**
     * Remove um dia do disco.
     */
    public boolean deleteDay(String date) {
        lock.lock();
        try {
            Path dayFile = storePath.resolve(date + ".events");
            return Files.deleteIfExists(dayFile);
        } catch (IOException e) {
            System.err.println("[FileSeriesStore] Erro ao deletar dia " + date + ": " + e.getMessage());
            return false;
        } finally {
            lock.unlock();
        }
    }

    /**
     * Remove todos os dias armazenados (limpeza).
     */
    public void clear() {
        lock.lock();
        try {
            try (DirectoryStream<Path> stream = Files.newDirectoryStream(storePath, "*.events")) {
                for (Path path : stream) {
                    Files.delete(path);
                }
            } catch (IOException e) {
                System.err.println("[FileSeriesStore] Erro ao limpar armazém: " + e.getMessage());
            }
        } finally {
            lock.unlock();
        }
    }

    /**
     * Obtem o caminho do armazém.
     */
    public Path getStorePath() {
        return storePath;
    }
}

interface EventProcessor {
    void process(List<Event> chunk);
}
