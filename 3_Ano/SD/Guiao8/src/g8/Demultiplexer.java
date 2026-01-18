package g8;

import g8.TaggedConnection.Frame;
import java.io.IOException;
import java.util.*;
import java.util.concurrent.locks.*;

public class Demultiplexer implements AutoCloseable {
    private final TaggedConnection conn;
    private final ReentrantLock lock = new ReentrantLock();
    private final Map<Integer, Entry> queues = new HashMap<>();
    private Exception exception = null;
    private Thread reader = null;

    public static class Entry {
        public final Deque<byte[]> queue = new ArrayDeque<>();
        public final ReentrantLock lock = new ReentrantLock();
        public final Condition notEmpty = lock.newCondition();
        public int waiters = 0;
    }

    private String t() { return Thread.currentThread().getName(); }

    public Demultiplexer(TaggedConnection conn) {
        this.conn = conn;
    }

    public void start() {
        lock.lock();
        try {
            if (reader != null)
                throw new IllegalStateException("Already started");

            reader = new Thread(() -> {
                try {

                    while (true) {
                        Frame f = conn.receive();
                        int tag = f.tag;

                        Entry entry;
                        lock.lock();
                        try {
                            entry = queues.get(tag);
                            // criar entry se não existir
                            if (entry == null) {
                                entry = new Entry();
                                queues.put(tag, entry);
                            }
                        } finally {
                            lock.unlock();
                        }

                        entry.lock.lock();
                        try {
                            // colocar dados na queue da entry se existir
                            entry.queue.add(f.data);
                            entry.notEmpty.signal();
                        } finally {
                            entry.lock.unlock();
                        }
                    }

                } catch (Exception e) {
                    // O reader morreu -> guardar exceção
                    lock.lock();
                    try {
                        exception = e;
                        // Acordar TODAS as threads que estão à espera
                        for (Entry entry : queues.values()) {
                            entry.lock.lock();
                            try {
                                entry.notEmpty.signalAll();
                            } finally {
                                entry.lock.unlock();
                            }
                        }
                    } finally {
                        lock.unlock();
                    }
                }
            }, "Demultiplexer-Reader");

            reader.setDaemon(true);
            reader.start();

        } finally {
            lock.unlock();
        }
    }

    public void send(int tag, byte[] data) throws IOException {
        conn.send(tag, data);
    }

    public void send(Frame f) throws IOException {
        conn.send(f);
    }

    public byte[] receive(int tag) throws IOException, InterruptedException {
        Entry entry;

        // criar ou obter entry
        lock.lock();
        try {
            entry = queues.get(tag);
            if (entry == null) {
                entry = new Entry();
                queues.put(tag, entry);
            }
            entry.lock.lock();
        } finally {
            lock.unlock();
        }

        try {
            while (entry.queue.isEmpty()) {

                // antes de bloquear ─> ver se o reader morreu
                lock.lock();
                try {
                    if (exception != null)
                        throw new IOException("Reader failed", exception);
                } finally {
                    lock.unlock();
                }

                entry.waiters++;
                try {
                    entry.notEmpty.await();  // esperar mensagem do tag
                } finally {
                    entry.waiters--;
                }

                // acordou — verificar se foi acordado por falha no reader
                lock.lock();
                try {
                    if (exception != null)
                        throw new IOException("Reader failed", exception);
                } finally {
                    lock.unlock();
                }
            }

            // Temos mensagem
            byte[] data = entry.queue.poll();

            // Após consumir: remover entry se:
            // - não há mais mensagens???
            // - não há mais waiters
            if (entry.queue.isEmpty() && entry.waiters == 0) {
                lock.lock();
                try {
                    if (entry.queue.isEmpty() && entry.waiters == 0)
                        queues.remove(tag);
                } finally {
                    lock.unlock();
                }
            }

            return data;

        } finally {
            entry.lock.unlock();
        }
    }

    @Override
    public void close() throws IOException {
        Thread r;
        lock.lock();
        try {
            r = reader;
            reader = null;
        } finally {
            lock.unlock();
        }
        if (r != null) r.interrupt();
        conn.close();
    }

}
