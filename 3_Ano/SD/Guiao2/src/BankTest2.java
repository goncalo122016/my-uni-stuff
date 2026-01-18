import java.util.Random;

public class BankTest2 {
    public static class Mover implements Runnable {
        private Bank b;
        private int accs;
        private int iters;

        public Mover(Bank b, int accs, int iters) {
            this.b = b;
            this.accs = accs;
            this.iters = iters;
        }

        public void run() {
            Random rand = new Random();
            for (int i = 0; i < iters; i++) {
                int acc = rand.nextInt(accs);
                if (rand.nextBoolean()) {
                    b.deposit(acc, 1);
                } else {
                    b.withdraw(acc, 1);
                }
            }
        }
    }

    public static void main(String[] args) throws InterruptedException {
        int ACCS = 10;
        int ITERS = 100000;
        int THREADS = 4;
        int INIT_BAL = 1000;
        Bank b = new Bank(ACCS);
        for (int i = 0; i < ACCS; i++)
            b.deposit(i, INIT_BAL);

        long start = System.currentTimeMillis();
        Thread[] ts = new Thread[THREADS];
        for (int i = 0; i < THREADS; i++) {
            ts[i] = new Thread(new Mover(b, ACCS, ITERS));
            ts[i].start();
        }
        for (Thread t : ts) t.join();
        long end = System.currentTimeMillis();

        int total = b.totalBalance();
        int expected = ACCS * INIT_BAL; // Deposits and withdrawals cancel out
        System.out.println("Expected: " + expected + ", Actual: " + total);
        if (total != expected)
            System.out.println("Race condition detected!");
        else
            System.out.println("Test OK");

        System.out.println("\nElapsed time: " + (end - start) + " ms");
    }
}
