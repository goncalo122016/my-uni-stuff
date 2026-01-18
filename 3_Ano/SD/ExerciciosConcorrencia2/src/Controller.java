import java.util.concurrent.locks.Condition;
import java.util.concurrent.locks.ReentrantLock;

public class Controller implements IController {
    private final ReentrantLock lock = new ReentrantLock(true); // justo ⇒ reduz starvation

    private final int T;  // máximo de threads por recurso
    private int activeResource = -1; // -1 = nenhum ativo, 0 ou 1 = recurso atual
    private final int[] usingCount = new int[2]; // nº threads a usar recurso i

    // Conditions para filas separadas por recurso
    private final Condition[] waiting = new Condition[2];

    public Controller(int T) {
        this.T = T;
        waiting[0] = lock.newCondition();
        waiting[1] = lock.newCondition();
    }

    @Override
    public int request_resource(int i) {
        lock.lock();
        try {
            while (
                // Outro recurso ativo
                    (activeResource != -1 && activeResource != i) ||
                            // Este recurso já atingiu o limite de T threads
                            (activeResource == i && usingCount[i] == T)
            ) {
                waiting[i].await();
            }

            // Se ninguém estava ativo, agora este passa a ser o ativo
            if (activeResource == -1)
                activeResource = i;

            usingCount[i]++;
            return i;

        } catch (InterruptedException e) {
            Thread.currentThread().interrupt();
            return -1;
        } finally {
            lock.unlock();
        }
    }

    @Override
    public void release_resource(int i) {
        lock.lock();
        try {
            usingCount[i]--;

            // Se nenhuma thread mais está a usar o recurso, libertamos o canal
            if (usingCount[i] == 0) {
                activeResource = -1; // permite outro recurso tornar-se ativo
                // Acorda *todas* as filas (para evitar starvation entre recursos)
                waiting[0].signalAll();
                waiting[1].signalAll();
            } else {
                // Ainda há threads do mesmo recurso, acordamos só as desse recurso
                waiting[i].signal();
            }

        } finally {
            lock.unlock();
        }
    }
}