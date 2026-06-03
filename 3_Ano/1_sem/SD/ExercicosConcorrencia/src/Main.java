import java.util.concurrent.ThreadLocalRandom;
import java.util.concurrent.TimeUnit;

public class Main {
    public static void main(String[] args) throws Exception {
        IJogo jogo = new Jogo();
        final int nThreads = 3;
        Thread[] threads = new Thread[nThreads];

        System.out.println("Waiting before starting the partida...");

        for (int t = 0; t < nThreads; t++) {
            final int id = t + 1;
            threads[t] = new Thread(() -> {
                try {
                    Partida p = jogo.participa();
                    System.out.println("Jogador-" + id + " entered partida: " + p);

                    for (int i = 0; i < 200; i++) {
                        int guess = ThreadLocalRandom.current().nextInt(1, 41);
                        String res = p.adivinha(guess);
                        String out = String.format("Jogador-%d guess=%d -> %s", id, guess, res);
                        System.out.println(out);
                        if ("GANHOU".equals(res) || "TENTATIVAS".equals(res) || "TEMPO".equals(res) || "PERDEU".equals(res)) {
                            break;
                        }
                        // small pause to avoid spamming
                        TimeUnit.MILLISECONDS.sleep(50);
                    }
                } catch (Exception e) {
                    System.err.println("Thread-" + id + " error: " + e);
                }
            }, "Player-" + id);
            threads[t].start();
        }

        // Give threads time to reach participa() and wait
        Thread.sleep(300);
    }
}
