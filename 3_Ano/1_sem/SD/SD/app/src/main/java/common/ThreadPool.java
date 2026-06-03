package common;

import java.util.ArrayDeque;
import java.util.ArrayList;
import java.util.Deque;
import java.util.List;
import java.util.concurrent.locks.Condition;
import java.util.concurrent.locks.ReentrantLock;

public class ThreadPool {

    private final Deque<Runnable> queue = new ArrayDeque<>();
    private final ReentrantLock lock = new ReentrantLock();
    private final Condition notEmpty = lock.newCondition();
    private final List<Thread> workers = new ArrayList<>();

    public ThreadPool(int nThreads) {
        for (int i = 0; i < nThreads; i++) {
            Thread t = new Thread(() -> {
                try {
                    while (true) {
                        Runnable task;
                        lock.lock();
                        try {
                            while (queue.isEmpty()) {
                                notEmpty.await();
                            }
                            task = queue.pollFirst();
                        } finally {
                            lock.unlock();
                        }
                        task.run();
                    }
                } catch (InterruptedException e) {
                }
            }, "PoolWorker-" + i);

            t.start();
            workers.add(t);
        }
    }

    public void submit(Runnable task) {
        lock.lock();
        try {
            queue.addLast(task);
            notEmpty.signal();
        } finally {
            lock.unlock();
        }
    }

    public void shutdown() {
        for (Thread t : workers) {
            t.interrupt();
        }
    }
}
