import java.util.concurrent.locks.Lock;
import java.util.concurrent.locks.ReentrantLock;

public class Bank extends ReentrantLock {

    private static class Account {
        private int balance;
        private Lock l = new ReentrantLock();
        Account(int balance) { this.balance = balance; }

        public void lockA() { l.lock(); }
        public void unlockA() { l.unlock(); }

        int balance() {
            int b;
            l.lock();
            try {
                b = balance;
            }
            finally {
                l.unlock();
            }
            return b;
        }

        boolean deposit(int value) {
            if (value < 0)
                return false;
            l.lock();
            try {
                balance += value;
                return true;
            } finally {
                l.unlock();
            }
        }

        boolean withdraw(int value) {
            if (value > balance)
                return false;
            l.lock();
            try {
                balance -= value;
                return true;
            } finally {
                l.unlock();
            }
        }
    }

    // Bank slots and vector of accounts
    private final int slots;
    private Account[] av;

    public Bank(int n) {
        slots=n;
        av=new Account[slots];
        for (int i=0; i<slots; i++) av[i]=new Account(0);
    }

    // Account balance
    public int balance(int id) {
        if (id < 0 || id >= slots)
            return 0;
        return av[id].balance();
    }

    // Deposit
    public boolean deposit(int id, int value) {
        if (id < 0 || id >= slots)
            return false;
        return av[id].deposit(value);
    }

    // Withdraw; fails if no such account or insufficient balance
    public boolean withdraw(int id, int value) {
        if (id < 0 || id >= slots)
            return false;
        return av[id].withdraw(value);
    }

    // Transfer; fails if no such account or insufficient balance
    public boolean transfer(int from, int to, int value) {
        if (from < 0 || from >= slots || to < 0 || to >= slots)
            return false;

        Account first = av[Math.min(from, to)];
        Account second = av[Math.max(from, to)];

        first.lockA();
        second.lockA();
        try {
            if (av[from].balance() < value)
                return false;
            av[from].withdraw(value);
            av[to].deposit(value);
            return true;
        } finally {
            first.unlockA();
            second.unlockA();
        }
    }

    // Total balance of all accounts
    public int totalBalance() {
        int total = 0;
        for (int i = 0; i < slots; i++)
            av[i].lockA();

        for (int i = 0; i < slots; i++) {
            total += balance(i);
            av[i].unlockA();
        }

        return total;
    }
}
