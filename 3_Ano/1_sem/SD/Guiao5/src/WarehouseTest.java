import java.lang.reflect.Field;
import java.util.*;
import java.util.concurrent.*;
import java.util.concurrent.atomic.AtomicInteger;

public class WarehouseTest {

    private static class Supplier implements Runnable {
        private final Warehouse warehouse;
        private final List<String> items;
        private final int ops;
        private final CountDownLatch start;

        Supplier(Warehouse warehouse, List<String> items, int ops, CountDownLatch start) {
            this.warehouse = warehouse;
            this.items = items;
            this.ops = ops;
            this.start = start;
        }

        public void run() {
            Random rnd = new Random();
            try {
                start.await();
                for (int i = 0; i < ops; i++) {
                    String it = items.get(rnd.nextInt(items.size()));
                    int q = 1 + rnd.nextInt(5);
                    warehouse.supply(it, q);
                    Thread.sleep(rnd.nextInt(3));
                }
            } catch (InterruptedException e) {
                Thread.currentThread().interrupt();
            }
        }
    }

    private static class Consumer implements Runnable {
        private final Warehouse warehouse;
        private final List<String> items;
        private final int ops;
        private final CountDownLatch start;
        private final AtomicInteger consumedCount;

        Consumer(Warehouse warehouse, List<String> items, int ops, CountDownLatch start, AtomicInteger consumedCount) {
            this.warehouse = warehouse;
            this.items = items;
            this.ops = ops;
            this.start = start;
            this.consumedCount = consumedCount;
        }

        public void run() {
            Random rnd = new Random();
            try {
                start.await();
                for (int i = 0; i < ops; i++) {
                    // choose 1 or 2 distinct items
                    Set<String> req = new HashSet<>();
                    req.add(items.get(rnd.nextInt(items.size())));
                    if (rnd.nextBoolean()) {
                        req.add(items.get(rnd.nextInt(items.size())));
                    }
                    warehouse.consume(req);
                    consumedCount.incrementAndGet();
                    Thread.sleep(rnd.nextInt(3));
                }
            } catch (InterruptedException e) {
                Thread.currentThread().interrupt();
            }
        }
    }

    public static void main(String[] args) throws Exception {
        final Warehouse warehouse = new Warehouse();
        final List<String> items = Arrays.asList("A", "B", "C", "D");

        // initial supply so consumers have something to take
        for (String it : items) warehouse.supply(it, 100);

        final int SUPPLIERS = 4;
        final int CONSUMERS = 8;
        final int SUP_OPS = 2000;
        final int CON_OPS = 2000;

        ExecutorService exec = Executors.newFixedThreadPool(SUPPLIERS + CONSUMERS);
        CountDownLatch startLatch = new CountDownLatch(1);
        List<Future<?>> futures = new ArrayList<>();
        AtomicInteger consumeAttempts = new AtomicInteger();

        for (int i = 0; i < SUPPLIERS; i++) {
            futures.add(exec.submit(new Supplier(warehouse, items, SUP_OPS, startLatch)));
        }
        for (int i = 0; i < CONSUMERS; i++) {
            futures.add(exec.submit(new Consumer(warehouse, items, CON_OPS, startLatch, consumeAttempts)));
        }

        long t0 = System.currentTimeMillis();
        startLatch.countDown();

        exec.shutdown();
        boolean finished = exec.awaitTermination(30, TimeUnit.SECONDS);
        if (!finished) {
            exec.shutdownNow();
            throw new RuntimeException("Threads did not finish in time");
        }

        // propagate exceptions from tasks
        for (Future<?> f : futures) {
            try {
                f.get(1, TimeUnit.SECONDS);
            } catch (ExecutionException ee) {
                throw new RuntimeException("Task threw exception", ee.getCause());
            }
        }

        long t1 = System.currentTimeMillis();

        // Inspect private map and product quantities
        Field mapField = Warehouse.class.getDeclaredField("map");
        mapField.setAccessible(true);
        @SuppressWarnings("unchecked")
        Map<String, Object> internal = (Map<String, Object>) mapField.get(warehouse);

        boolean ok = true;
        System.out.println("\nFinal warehouse state:");
        for (Map.Entry<String, Object> e : internal.entrySet()) {
            Object product = e.getValue();
            Field qtyField = product.getClass().getDeclaredField("quantity");
            qtyField.setAccessible(true);
            int qty = qtyField.getInt(product);
            System.out.println("Item " + e.getKey() + " -> " + qty);
            if (qty < 0) ok = false;
        }

        System.out.println("\nConsume attempts: " + consumeAttempts.get());
        System.out.println("Elapsed ms: " + (t1 - t0));
        if (ok) {
            System.out.println("Test OK: no negative quantities detected");
        } else {
            System.out.println("Test FAILED: negative quantity detected");
            throw new AssertionError("Negative quantity detected in Warehouse");
        }
    }
}
