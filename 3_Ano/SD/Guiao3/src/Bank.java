import java.util.*;
import java.util.concurrent.locks.Lock;
import java.util.concurrent.locks.ReentrantLock;
import java.util.concurrent.locks.ReentrantReadWriteLock;

class Bank {

    private static class Account {
        private int balance;
        private final Lock lock = new ReentrantLock();

        Account(int balance) { this.balance = balance; }
        int balance() { return balance; }
        boolean deposit(int value) {
            balance += value;
            return true;
        }
        boolean withdraw(int value) {
            if (value > balance)
                return false;
            balance -= value;
            return true;
        }
        void lock() { lock.lock(); }
        void unlock() { lock.unlock(); }
    }

    private final Map<Integer, Account> map = new HashMap<>();
    private int nextId = 0;
    private final Lock bankLockWrite = new ReentrantReadWriteLock.WriteLock();
    private final Lock bankLockRead = new ReentrantReadWriteLock.ReadLock();
    private final Lock bankLock = new ReentrantLock();

    public int createAccount(int balance) {
        bankLock.lock();
        try {
            Account c = new Account(balance);
            int id = nextId++;
            map.put(id, c);
            return id;
        } finally {
            bankLock.unlock();
        }
    }

    public int closeAccount(int id) {
        bankLock.lock();
        try {
            Account c = map.remove(id);
            if (c == null)
                return 0;
            c.lock();
            try {
                return c.balance();
            } finally {
                c.unlock();
            }
        } finally {
            bankLock.unlock();
        }
    }

    public int balance(int id) {
        Account c;
        bankLock.lock();
        try {
            c = map.get(id);
            if (c == null)
                return 0;
            c.lock();
        } finally {
            bankLock.unlock();
        }
        try {
            return c.balance();
        } finally {
            c.unlock();
        }
    }

    public boolean deposit(int id, int value) {
        Account c;
        bankLock.lock();
        try {
            c = map.get(id);
            if (c == null)
                return false;
            c.lock();
        } finally {
            bankLock.unlock();
        }
        try {
            return c.deposit(value);
        } finally {
            c.unlock();
        }
    }

    public boolean withdraw(int id, int value) {
        Account c;
        bankLock.lock();
        try {
            c = map.get(id);
            if (c == null)
                return false;
            c.lock();
        } finally {
            bankLock.unlock();
        }
        try {
            return c.withdraw(value);
        } finally {
            c.unlock();
        }
    }

    public boolean transfer(int from, int to, int value) {
        Account cfrom, cto;
        bankLock.lock();
        try {
            cfrom = map.get(from);
            cto = map.get(to);
            if (cfrom == null || cto == null)
                return false;

            if (from < to) {
                cfrom.lock();
                cto.lock();
            } else if (from > to) {
                cto.lock();
                cfrom.lock();
            } else {
                cfrom.lock();
            }
        } finally {
            bankLock.unlock();
        }
        try {
            if (cfrom == null || cto == null)
                return false;
            if (!cfrom.withdraw(value))
                return false;
            cto.deposit(value);
            return true;
        } finally {
            if (from == to) {
                cfrom.unlock();
            } else {
                cfrom.unlock();
                cto.unlock();
            }
        }
    }

    public int totalBalance(int[] ids) {
        List<Account> accounts = new ArrayList<>();
        bankLock.lock();
        try {
            for (int i : ids) {
                Account c = map.get(i);
                if (c == null)
                    return 0;
                accounts.add(c);
            }

            accounts.sort(Comparator.comparingInt(System::identityHashCode));
            for (Account c : accounts) {
                c.lock();
            }
        } finally {
            bankLock.unlock();
        }
        try {
            int total = 0;
            for (Account c : accounts) {
                total += c.balance();
            }
            return total;
        } finally {
            for (Account c : accounts) {
                c.unlock();
            }
        }
    }
}
