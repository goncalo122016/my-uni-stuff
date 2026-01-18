package protocol;

import java.io.IOException;
import java.util.*;
import java.util.concurrent.locks.*;

public class Demultiplexer implements AutoCloseable {

    private final TaggedConnection conn;
    private final ReentrantLock lock = new ReentrantLock();
    private final Map<Integer, Entry> queues = new HashMap<>();
    private Exception exception = null;
    private Thread reader = null;

    private static class Entry {
        final Deque<Frame> queue = new ArrayDeque<>();
        final ReentrantLock lock = new ReentrantLock();
        final Condition notEmpty = lock.newCondition();
        int waiters = 0;
    }

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
                        int tag = f.tag();

                        Entry entry;
                        lock.lock();
                        try {
                            entry = queues.computeIfAbsent(tag, k -> new Entry());
                        } finally {
                            lock.unlock();
                        }

                        entry.lock.lock();
                        try {
                            entry.queue.add(f);
                            entry.notEmpty.signal();
                        } finally {
                            entry.lock.unlock();
                        }
                    }

                } catch (Exception e) {
                    lock.lock();
                    try {
                        exception = e;

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

    public void send(int tag, MsgType type, byte[] data) throws IOException {
        conn.send(tag, type, data);
    }

    public void send(Frame f) throws IOException {
        conn.send(f);
    }

    public Frame receive(int tag) throws IOException, InterruptedException {
        Entry entry;

        lock.lock();
        try {
            entry = queues.computeIfAbsent(tag, unused -> new Entry());
            entry.lock.lock();
        } finally {
            lock.unlock();
        }

        try {
            while (entry.queue.isEmpty()) {

                lock.lock();
                try {
                    if (exception != null)
                        throw new IOException("Reader failed", exception);
                } finally {
                    lock.unlock();
                }

                entry.waiters++;
                try {
                    entry.notEmpty.await();
                } finally {
                    entry.waiters--;
                }
            }

            Frame f = entry.queue.poll();

            if (entry.queue.isEmpty() && entry.waiters == 0) {
                lock.lock();
                try {
                    if (entry.queue.isEmpty() && entry.waiters == 0)
                        queues.remove(tag);
                } finally {
                    lock.unlock();
                }
            }

            return f;

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
