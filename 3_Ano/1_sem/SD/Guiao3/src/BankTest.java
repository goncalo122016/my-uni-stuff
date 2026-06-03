import java.util.*;
import java.util.concurrent.CopyOnWriteArrayList;

public class BankTest {

    private static class Mover implements Runnable {
        private final Bank bank;
        private final List<Integer> ids;
        private final int iters;

        public Mover(Bank bank, List<Integer> ids, int iters) {
            this.bank = bank;
            this.ids = ids;
            this.iters = iters;
        }

        public void run() {
            Random rand = new Random();
            for (int m = 0; m < iters; m++) {
                List<Integer> snapshot = new ArrayList<>(ids);
                if (snapshot.size() < 2) continue;
                int fromIdx = rand.nextInt(snapshot.size());
                int toIdx = rand.nextInt(snapshot.size());
                if (fromIdx == toIdx) continue;
                int from = snapshot.get(fromIdx);
                int to = snapshot.get(toIdx);
                bank.transfer(from, to, 1);
            }
        }
    }

    private static class Closer implements Runnable {
        private final Bank bank;
        private final List<Integer> ids;
        private final List<Integer> closedBalances;

        public Closer(Bank bank, List<Integer> ids, List<Integer> closedBalances) {
            this.bank = bank;
            this.ids = ids;
            this.closedBalances = closedBalances;
        }

        public void run() {
            Random rand = new Random();
            for (int i = 0; i < 5; i++) { // Try to close 5 accounts
                if (ids.isEmpty()) break;
                int idx = rand.nextInt(ids.size());
                int id = ids.remove(idx);
                int closed = bank.closeAccount(id);
                closedBalances.add(closed);
                System.out.println("Closed account " + id + ", got: " + closed);
            }
        }
    }

    private static class Creator implements Runnable {
        private final Bank bank;
        private final List<Integer> ids;
        private final int count;
        private final int initialBalance;

        public Creator(Bank bank, List<Integer> ids, int count, int initialBalance) {
            this.bank = bank;
            this.ids = ids;
            this.count = count;
            this.initialBalance = initialBalance;
        }

        public void run() {
            for (int i = 0; i < count; i++) {
                int id = bank.createAccount(initialBalance);
                ids.add(id);
                System.out.println("Created account " + id + " with " + initialBalance);
            }
        }
    }

    public static void main(String[] args) throws InterruptedException {
        int ACCS = 10;
        int ITERS = 10000000;
        int CREATES = 5;
        int INITIAL_BALANCE = 1000;

        Bank bank = new Bank();
        List<Integer> ids = new CopyOnWriteArrayList<>();
        List<Integer> closedBalances = new ArrayList<>();

        for (int i = 0; i < ACCS; i++) {
            ids.add(bank.createAccount(INITIAL_BALANCE));
        }

        int expected = ACCS * INITIAL_BALANCE;

        List<Runnable> runnables = Arrays.asList(
                new Mover(bank, ids, ITERS),
                new Closer(bank, ids, closedBalances),
                new Creator(bank, ids, CREATES, INITIAL_BALANCE)
        );

        // Shuffle and assign runnables to threads
        List<Thread> threads = new ArrayList<>();
        List<Runnable> shuffled = new ArrayList<>(runnables);
        Collections.shuffle(shuffled);
        for (Runnable r : shuffled) {
            threads.add(new Thread(r));
        }

        long start = System.currentTimeMillis();
        for (Thread t : threads) t.start();
        for (Thread t : threads) t.join();
        long end = System.currentTimeMillis();

        // Calculate expected total
        expected += CREATES * INITIAL_BALANCE;
        for (int closed : closedBalances) expected -= closed;

        // Print all account balances
        int realTotal = 0;
        System.out.println("\nAccount balances:");
        for (int id : ids) {
            int bal = bank.balance(id);
            System.out.println("Account " + id + ": " + bal);
            realTotal += bal;
        }
        System.out.println("Expected total: " + expected);
        System.out.println("Real total: " + realTotal);

        if (expected == realTotal)
            System.out.println("Test OK");
        else
            System.out.println("Unexpected balance");

        System.out.println("\nElapsed time: " + (end - start) + " ms");
    }
}
