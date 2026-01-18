import java.util.*;
import java.util.concurrent.locks.*;

public class ManagerImpl implements IManager {

    private final ReentrantLock lock = new ReentrantLock();
    private final Map<String, Transaction> transactions = new HashMap<>();
    private int nextId = 0;

    // transação de manutenção (no máx. 1)
    private Transaction maintenance = null;

    @Override
    public String begin(Set<String> keySet) throws InterruptedException {
        lock.lock();
        try {
            String id = Integer.toString(nextId++);
            Condition cond = lock.newCondition();
            Transaction t = new Transaction(id, keySet, cond);
            transactions.put(id, t);

            if (keySet == null) { // manutenção
                while (maintenance != null || existsExecutingTransaction()) {
                    cond.await();
                }
                maintenance = t;
            } else {
                while (conflictsWithExecuting(t) || maintenance != null) {
                    cond.await();
                }
            }

            t.status = 'e';
            return id;
        } finally {
            lock.unlock();
        }
    }

    @Override
    public void commit(String id) {
        lock.lock();
        try {
            Transaction t = transactions.get(id);
            if (t == null) return;

            t.status = 'c';

            if (t == maintenance) {
                maintenance = null;
            }

            // acordar apenas quem pode avançar
            for (Transaction other : transactions.values()) {
                if (other.status == 'b' &&
                        !conflictsWithExecuting(other) &&
                        (maintenance == null || other.keys == null)) {
                    other.cond.signal();
                }
            }
        } finally {
            lock.unlock();
        }
    }

    @Override
    public char getTransactionStatus(String id) {
        lock.lock();
        try {
            Transaction t = transactions.get(id);
            return (t == null) ? ' ' : t.status;
        } finally {
            lock.unlock();
        }
    }

    /* ----------------- Métodos auxiliares ----------------- */

    private boolean existsExecutingTransaction() {
        for (Transaction t : transactions.values())
            if (t.status == 'e')
                return true;
        return false;
    }

    private boolean conflictsWithExecuting(Transaction t) {
        for (Transaction other : transactions.values()) {
            if (other != t && other.status == 'e' && t.conflictsWith(other))
                return true;
        }
        return false;
    }
}
