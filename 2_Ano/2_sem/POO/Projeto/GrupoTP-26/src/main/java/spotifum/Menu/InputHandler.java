package spotifum.Menu;

import java.util.ArrayList;
import java.util.List;
import java.util.Scanner;

import spotifum.Musics.Musica;
import spotifum.Musics.implementations.GeneroMusical;
import spotifum.Musics.implementations.TipoMusica;
import spotifum.Users.PlanoSubscricao;

/**
 * Class responsible for managing user input data.
 */
public class InputHandler {
    private final Scanner scanner;

    /**
     * InputHandler class constructor.
     *
     * @param scanner The Scanner object for input reading.
     */
    public InputHandler(Scanner scanner) {
        this.scanner = scanner;
    }

    /**
     * Reads an integer from standard input.
     *
     * @param message The message to be displayed to the user.
     * @return The integer value read.
     */
    public int readInt(String message) {
        System.out.print(message);
        while (!scanner.hasNextInt()) {
            System.out.println("Por favor introduza um número válido.");
            scanner.next();
        }
        int value = scanner.nextInt();
        scanner.nextLine(); // consumir newline
        return value;
    }

    /**
     * Reads a line from standard input.
     *
     * @param message The message to be displayed to the user.
     * @return The line read.
     */
    public String readLine(String message) {
        System.out.print(message);
        return scanner.nextLine();
    }

    /**
     * Reads multiple lines from standard input until finding two consecutive empty lines.
     *
     * @return A list with the lines read.
     */
    public List<String> readMuitasLinhas() {
        List<String> linhas = new ArrayList<>();
        String linha = scanner.nextLine();

        while (!linha.isEmpty() || linhas.isEmpty()) {
            linhas.add(linha);
            linha = scanner.nextLine();
        }

        if (!linhas.isEmpty() && linhas.get(linhas.size() - 1).isEmpty()) {
            linhas.remove(linhas.size() - 1);
        }

        return linhas;
    }

    /**
     * Reads multiple lines from standard input but only valid Musics.
     *
     * @param allMusics List of all valid music titles.
     * @return A list with the valid music titles read.
     */
    public List<String> readMuitasMusicas(List<String> allMusics) {
        List<String> musicas = new ArrayList<>();
        String linha;

        while (true) {
            linha = scanner.nextLine().trim();

            if (linha.isEmpty()) {
                break;
            }

            if (!allMusics.contains(linha)) {
                System.out.println("A música '" + linha + "' não existe! Tente novamente.");
                continue;
            }

            musicas.add(linha);
        }

        return musicas;
    }

    /**
     * Reads a boolean response (Y/N) from standard input.
     *
     * @return true if the answer is 'S', false if it is 'N'.
     */
    public boolean readBool() {
        while (true){
            String input = readLine("S/N: ");
            if (input.equalsIgnoreCase("S")) {
                return true;
            }
            if (input.equalsIgnoreCase("N")) {
                return false;
            }
            System.out.println("Insira apenas S ou N. Tente novamente.");
        }
    }

    /**
     * Reads a subscription plan from standard input.
     *
     * @return The subscription plan read.
     */
    public PlanoSubscricao readPlanoSubscricao() {
        while (true) {
            System.out.print("Plano de Subscrição (FREE, PremiumBase, PremiumTop): ");
            String input = readLine("");

            for (PlanoSubscricao plano : PlanoSubscricao.values()) {
                if (plano.name().equalsIgnoreCase(input)) {
                    return plano;
                }
            }

            System.out.println("Plano inválido. Tente novamente.");
        }
    }

    /**
     * Reads a musical genre from standard input.
     *
     * @return The musical genre read.
     */
    public GeneroMusical readGeneroMusical() {
        while (true) {
            System.out.print("Género (Pop, Rock, Clássica, Jazz, HipHop, Eletrónica, Reggae, Fado ou Outro): ");
            String input = readLine("");

            for (GeneroMusical genero : GeneroMusical.values()) {
                if (genero.name().equalsIgnoreCase(input)) {
                    return genero;
                }
            }

            System.out.println("Género inválido. Tente novamente.");
        }
    }

    /**
     * Reads a music type from standard input.
     *
     * @return The music type read.
     */
    public TipoMusica readTipoMusica() {
        while (true) {
            String input = readLine("Tipo da Música (Explícita, Normal, Multimédia): ");

            for (TipoMusica tipo : TipoMusica.values()){
                if(tipo.name().equalsIgnoreCase(input)){
                    return tipo;
                }
            }

            System.out.println("Tipo inválido. Tente novamente.");
        }
    }

    /**
     * Displays the music currently playing with formatted output.
     *
     * @param music The music object to display.
     */
    public void displayMusicPlaying(Musica music) {
        // Cabeçalho com informações da música
        System.out.println("\n▶ A reproduzir: " + music.getNome());
        System.out.println("🎤 " + music.getInterprete() + " | 🎼 " + music.getGeneroMusical() + " | ⏱ " + music.getDuracaoSegundos() + "s\n");

        try {
            // Divide a letra em palavras
            String[] palavras = music.getLetra().split("\\s+");
            int palavrasPorLinha = 4;

            // Processa as palavras em grupos
            for (int i = 0; i < palavras.length; i += palavrasPorLinha) {
                StringBuilder linha = new StringBuilder("♪ ");

                for (int j = 0; j < palavrasPorLinha && i + j < palavras.length; j++) {
                    linha.append(palavras[i + j]).append(" ");
                }

                System.out.println(linha.toString().trim());
                Thread.sleep(500);
            }

        } catch (InterruptedException e) {
            Thread.currentThread().interrupt();
            System.out.println(music.getLetra());
        }
    }
}
