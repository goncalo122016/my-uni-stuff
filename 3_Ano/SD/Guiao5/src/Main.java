import java.util.ArrayList;
import java.util.List;
import java.util.Set;

import static java.lang.Thread.sleep;

public class Main {
    public static void main(String[] args) {
        Warehouse w = new Warehouse();
        List<String> items = List.of("item1", "item2", "item3", "item4", "item5");
        Thread n1 = new Thread(() -> {
            w.consume(Set.of("item1"));
            System.out.println("T1 - Consumed item1");
            w.consume(Set.of("item2"));
            System.out.println("T1 - Consumed item2");
        });
        Thread n2 = new Thread(() -> {
            w.supply("item1", 2);
            try {
                Thread.sleep(5000);
            } catch (InterruptedException e) {
                throw new RuntimeException(e);
            }
            w.supply("item2", 5);
        });
        Thread t3 = new Thread(() -> {
            w.consume(Set.of("item1"));
            System.out.println("T3 - Consumed item1");
        });
        List<Thread> threads = new ArrayList<>();
        threads.add(n1);
        threads.add(n2);
        threads.add(t3);

        for (Thread t : threads) t.start();
        for (Thread t : threads) {
            try {
                t.join();
            } catch (InterruptedException e) {
                Thread.currentThread().interrupt();
            }
        }
    }
}