import java.util.concurrent.locks.Condition;
import java.util.concurrent.locks.ReentrantLock;

public class Jogo implements IJogo {
    private final ReentrantLock lockJ = new ReentrantLock();
    private final Condition condJ = lockJ.newCondition();
    private final int maxJogadores = 6;
    private final int minJogadores = 4;
    private final int MAX_TENTATIVAS = 100;
    private int jogadores = 0;
    private Partida partidaAtual = null;
    private Thread timerThread = null;

    public Partida participa() {
        lockJ.lock();
        try {
            if (partidaAtual == null) {
                partidaAtual = new Partida(MAX_TENTATIVAS);
                iniciar_timer();
            }
            Partida partida = partidaAtual;
            jogadores++;

            if (jogadores == maxJogadores) {
                iniciarPartida();
            } else {
                while (!partida.getPronta()) {
                    try {
                        condJ.await();
                    } catch (InterruptedException e) {
                        Thread.currentThread().interrupt();
                    }
                }
            }
            return partida;
        } finally {
            lockJ.unlock();
        }
    }

    void iniciar_timer() {
        timerThread = new Thread(() -> {
            try {
                System.out.println("Starting 2 minute timer...");
                Thread.sleep(120_000); // wait 2 minutes
                lockJ.lock();
                try {
                    if (partidaAtual != null && !partidaAtual.getPronta() && jogadores >= 4) {
                        iniciarPartida();
                    }
                } finally {
                    lockJ.unlock();
                }
            } catch (InterruptedException e) {
                // timer cancelled because partida started early
            } finally {
                lockJ.lock();
                try {
                    timerThread = null;
                } finally {
                    lockJ.unlock();
                }
            }
        });
        timerThread.start();
    }

    void iniciarPartida() {
        if (timerThread != null) {
            timerThread.interrupt();
            timerThread = null;
        }
        partidaAtual.iniciar(jogadores);
        condJ.signalAll();
        partidaAtual = null;
        jogadores = 0;
    }
}
