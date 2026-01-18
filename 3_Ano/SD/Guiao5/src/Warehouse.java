import java.util.*;
import java.util.concurrent.locks.Condition;
import java.util.concurrent.locks.ReentrantLock;

class Warehouse {
    private Map<String, Product> map =  new HashMap<>();

    private class Product {
        int quantity = 0;
        final Condition cond = lock.newCondition();
    }
    private final ReentrantLock lock = new ReentrantLock();
    private final Condition cond = lock.newCondition();

    private Product get(String item) {
        Product p = map.get(item);
        if (p != null) return p;
        p = new Product();
        map.put(item, p);
        return p;
    }

    public void supply(String item, int quantity) {
        lock.lock();
        try {
            Product p = get(item);
            p.quantity += quantity;
            p.cond.signalAll();
        } finally {
            lock.unlock();
        }
    }

    public void consume(Set<String> items) {
        lock.lock();
        try {
            Iterator<String> it = items.iterator();
            while (it.hasNext()) {
                String s = it.next();
                Product p = get(s);
                while (p.quantity == 0) {
                    try {
                        p.cond.await();
                        it = items.iterator();
                    } catch (InterruptedException e) {
                        Thread.currentThread().interrupt();
                    }
                }
            }
            for (String s : items) {
                Product p = get(s);
                p.quantity--;
            }
        } finally {
            lock.unlock();
        }
    }
}
