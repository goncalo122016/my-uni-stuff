import java.util.concurrent.ThreadLocalRandom;

public class BarrierTest {
    public static void main(String[] args) {
        final int threads = 8;
        final int N = 1;

        Barrier barrier = new Barrier(N);

        Runnable task = () -> {
            String name = Thread.currentThread().getName();
                try {
                    Thread.sleep(ThreadLocalRandom.current().nextInt(10, 100));
                } catch (InterruptedException e) {
                    Thread.currentThread().interrupt();
                    return;
                }

                System.out.println(name + " waiting at barrier");
                try {
                    barrier.await();
                } catch (InterruptedException e) {
                    Thread.currentThread().interrupt();
                    System.out.println(name + " was interrupted while waiting");
                    return;
                }
                System.out.println(name + " crossed barrier");
        };

        Thread[] ts = new Thread[threads];
        for (int i = 0; i < threads; i++) {
            ts[i] = new Thread(task, "Thread-" + (i + 1));
            ts[i].start();
        }

        for (Thread t : ts) {
            try {
                t.join();
            } catch (InterruptedException e) {
                Thread.currentThread().interrupt();
                break;
            }
        }

        System.out.println("All threads finished.");
    }
}
