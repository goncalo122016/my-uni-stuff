//TIP To <b>Run</b> code, press <shortcut actionId="Run"/> or
// click the <icon src="AllIcons.Actions.Execute"/> icon in the gutter.
import java.util.ArrayList;
import java.util.List;

public class Main {
    private static class Worker implements Runnable {
        private final Barrier barrier;
        private final int id;

        public Worker(Barrier barrier, int id) {
            this.barrier = barrier;
            this.id = id;
        }

        public void run() {
            try {
                System.out.println("Thread " + id + " is waiting at the barrier.");
                Thread.sleep((long) (Math.random() * 1000));
                barrier.await();
                System.out.println("Thread " + id + " has crossed the barrier.");
            } catch (InterruptedException e) {
                Thread.currentThread().interrupt();
            }
        }
    }

    public static void main(String[] args) {
        int N_THREADS = 10;

        Barrier barrier = new Barrier(N_THREADS);

        List<Thread> threads = new ArrayList<>();
        for (int i = 0; i < N_THREADS; i++) {
            threads.add(new Thread(new Worker(barrier, i)));
        }
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