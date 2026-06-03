import java.time.LocalDateTime;
import java.util.concurrent.locks.ReentrantLock;

public class Partida implements IPartida {
    private final ReentrantLock lockP = new ReentrantLock();
    private int nJogadores;
    private int maxTentativas;
    private int nTentativas;
    private boolean pronta = false;
    private boolean terminado = false;
    private final int segredo = 20;
    private LocalDateTime inicio = null;

    public Partida(int maxT) {
        nJogadores = 0;
        maxTentativas = maxT;
        nTentativas = 0;
    }

    public int numeroJogadores() {
        return this.nJogadores;
    }

    public String adivinha(int n) {
        lockP.lock();
        try {
            if (inicio != null && LocalDateTime.now().isAfter(inicio.plusMinutes(1))) {
                return "TEMPO";
            }
            if (terminado) {
                return "PERDEU";
            }
            if (nTentativas >= maxTentativas) {
                return "TENTATIVAS";
            }
            if (n == segredo) {
                terminado = true;
                return "GANHOU";
            }
            if (n > segredo) {
                nTentativas++;
                return "MAIOR";
            }
            else {
                nTentativas++;
                return "MENOR";
            }
        }
        finally {
            lockP.unlock();
        }
    }

    public void iniciar(int nJogadores) {
        this.nJogadores = nJogadores;
        this.pronta = true;
        this.nTentativas = 0;
        this.terminado = false;
        this.inicio = LocalDateTime.now();
    }

    public boolean getPronta() {
        return this.pronta;
    }
}
