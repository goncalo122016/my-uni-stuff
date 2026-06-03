import java.util.concurrent.locks.Condition;
import java.util.concurrent.locks.ReentrantLock;

public class Barrier {
    private final ReentrantLock lock = new ReentrantLock();
    private final Condition cond = lock.newCondition();
    private final int N;
    private int waiters = 0;
    private int fase = 0;

    public Barrier(int n) {
        this.N = n;
    }

    public void await() throws InterruptedException {
        lock.lock();
        try {
            int faseAtual = fase;
            waiters++;
            if (waiters == N) {
                fase++;
                waiters = 0;
                cond.signalAll();
                return;
            }
            while (faseAtual == fase) {
                try {
                    cond.await();
                } catch (InterruptedException e) {
                    if (faseAtual == fase) {
                        waiters--;
                        cond.signalAll();
                    }
                    throw e;
                }
            }
        } finally {
            lock.unlock();
        }
    }
}
