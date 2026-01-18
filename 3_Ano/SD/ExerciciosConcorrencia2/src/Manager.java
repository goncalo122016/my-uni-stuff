import java.util.ArrayDeque;
import java.util.Queue;
import java.util.concurrent.locks.Condition;
import java.util.concurrent.locks.ReentrantLock;

public class Manager implements ManagerI {
    private final ReentrantLock l = new ReentrantLock();
    private final Condition cond = l.newCondition();

    public static final int R = 10;
    private int running = 0;
    private Queue<Raid> pending = new ArrayDeque<>();

    private Raid current = new Raid(this);
    private int maxMin = 0;

    public Raid join(String name, int minPlayers) throws InterruptedException {
        l.lock();
        try {
            Raid raid = current;
            raid.players().add(name);
            maxMin = Math.max(maxMin, minPlayers);
            if (raid.players().size() >= maxMin) {
                raid.init();
                tryStart(raid);
                System.out.printf("Raid formed with players=%s min=%d%n", raid.players(), maxMin);
                maxMin = 0;
                current = new Raid(this);
                cond.signalAll();
            }
            else {
                while (raid == current) {
                    cond.await();
                }
            }

            return raid;
        } finally {
            l.unlock();
        }
    }

    void tryStart(Raid raid) {
        if (running < R) {
            running += 1;
            raid.start();
        } else {
            pending.add(raid);
        }
    }

    void finished() {
        l.lock();
        try {
            running -= 1;
            Raid r = pending.poll();
            if (r != null)
                tryStart(r);
        } finally {
            l.unlock();
        }
    }


}
