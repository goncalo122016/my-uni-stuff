import java.util.ArrayList;
import java.util.Collections;
import java.util.List;
import java.util.concurrent.locks.Condition;
import java.util.concurrent.locks.ReentrantLock;

public class Raid implements RaidI {
    private ReentrantLock rl = new ReentrantLock();
    private Condition rcond = rl.newCondition();

    int playing;
    private List<String> players = new ArrayList<>();
    boolean started = false;

    Manager manager;

    public Raid(Manager manager) {
        this.manager = manager;
    }

    void init() {
        players = Collections.unmodifiableList(players);
        playing = players.size();
    }

    public List<String> players() {
        return players;
    }

    void start() {
        rl.lock();
        started = true;
        rcond.signalAll();
        rl.unlock();
    }

    public void waitStart() throws InterruptedException {
        rl.lock();
        try {
            while (!started) {
                rcond.await();
            }
        } finally {
            rl.unlock();
        }
    }

    public void leave() {
        rl.lock();
        playing -= 1;
        if (playing == 0) {
            manager.finished();
        }
        rl.unlock();
    }
}
