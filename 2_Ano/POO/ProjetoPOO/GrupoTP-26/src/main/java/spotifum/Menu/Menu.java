package spotifum.Menu;

import java.time.LocalDateTime;
import java.util.List;
import java.util.Scanner;

import spotifum.Musics.Musica;
import spotifum.Musics.implementations.GeneroMusical;
import spotifum.Playlists.Playlist;
import spotifum.Spotifum;
import spotifum.Users.Utilizador;

/**
 * Class responsible for menu management and user interface.
 */
public class Menu {
    private final Spotifum spotifum;
    private final InputHandler inputHandler;
    private final EntityCreator entityCreator;
    private final EntityViewer entityViewer;
    private final EntityRemover entityRemover;

    /**
     * Constructs a new Menu instance initializing required components.
     */
    public Menu() {
        Scanner scanner = new Scanner(System.in);
        this.spotifum = new Spotifum();
        this.inputHandler = new InputHandler(scanner);
        this.entityCreator = new EntityCreator(spotifum, inputHandler);
        this.entityViewer = new EntityViewer(spotifum);
        this.entityRemover = new EntityRemover(spotifum, inputHandler);
    }

    /**
     * Starts the main application menu loop.
     * Handles user input and directs to corresponding submenu or action.
     */
    public void start() {
        boolean running = true;

        while (running) {
            printMainMenu();
            int option = inputHandler.readInt("--> ");

            try {
                switch (option) {
                    case 1 -> submenuInserir();
                    case 2 -> submenuConsultar();
                    case 3 -> submenuApagar();
                    case 4 -> submenuReproducao();
                    case 5 -> guardarEstado();
                    case 6 -> carregarEstado();
                    case 7 -> estatisticas();
                    case 8 -> gerarPlaylist();
                    case 0 -> {
                        System.out.println("A sair... Obrigado por usar o Spotifum G-26!");
                        running = false;
                    }
                    default -> System.out.println("Opção inválida. Tente novamente.");
                }
            } catch (Exception e) {
                System.out.println("Erro: " + e.getMessage());
            }
        }
    }

    /**
     * Prints the main menu options to the console.
     */
    private void printMainMenu() {
        System.out.println("\n==== Spotifum G-26 - Menu Principal ====");
        System.out.println("1 - Inserir Entidade");
        System.out.println("2 - Consultar Entidade");
        System.out.println("3 - Apagar Entidade");
        System.out.println("4 - Registar Reprodução");
        System.out.println("5 - Guardar Estado para Ficheiro");
        System.out.println("6 - Carregar Estado de Ficheiro");
        System.out.println("7 - Estatísticas");
        System.out.println("8 - Gerar Playlist Automática");
        System.out.println("0 - Sair");
        System.out.println("====================================");
    }

    /**
     * Displays submenu for inserting entities and processes user selection.
     */
    private void submenuInserir() {
        System.out.println("\n-- Inserir Entidade --");
        System.out.println("1 - Utilizador");
        System.out.println("2 - Música");
        System.out.println("3 - Playlist");
        System.out.println("0 - Voltar");

        int option = inputHandler.readInt("-> ");
        switch (option) {
            case 1 -> entityCreator.criarUtilizador();
            case 2 -> entityCreator.criarMusica();
            case 3 -> entityCreator.criarPlaylist();
            case 0 -> { /* voltar */ }
            default -> System.out.println("Opção inválida.");
        }
    }

    /**
     * Displays submenu for consulting entities and processes user selection.
     */
    private void submenuConsultar() {
        System.out.println("\n-- Consultar Entidade --");
        System.out.println("1 - Utilizador");
        System.out.println("2 - Todos os Utilizadores");
        System.out.println("3 - Música");
        System.out.println("4 - Todas as Músicas");
        System.out.println("5 - Playlist");
        System.out.println("6 - Todas as Playlists");
        System.out.println("7 - Consultar o Histórico de Reproduções");
        System.out.println("0 - Voltar");

        int option = inputHandler.readInt("-> ");
        switch (option) {
            case 1 -> entityViewer.consultarUtilizador();
            case 2 -> entityViewer.consultarTodosUtilizadores();
            case 3 -> entityViewer.consultarMusica();
            case 4 -> entityViewer.consultarTodasMusicas();
            case 5 -> entityViewer.consultarPlaylist();
            case 6 -> entityViewer.consultarTodasPlaylist();
            case 7 -> entityViewer.consultarReproducoesUtilizador();
            case 0 -> { /* voltar */ }
            default -> System.out.println("Opção inválida.");
        }
    }

    /**
     * Displays submenu for deleting entities and processes user selection.
     */
    private void submenuApagar() {
        System.out.println("\n-- Apagar Entidade --");
        System.out.println("1 - Utilizador");
        System.out.println("2 - Música");
        System.out.println("3 - Playlist");
        System.out.println("0 - Voltar");

        int option = inputHandler.readInt("-> ");
        switch (option) {
            case 1 -> entityRemover.apagarUtilizador();
            case 2 -> entityRemover.apagarMusica();
            case 3 -> entityRemover.apagarPlaylist();
            case 0 -> { /* voltar */ }
            default -> System.out.println("Opção inválida.");
        }
    }

    /**
     * Displays submenu for registering playback and processes user selection.
     */
    private void submenuReproducao() {
        System.out.println("\n-- Reproduzir --");
        System.out.println("1 - Uma Música");
        System.out.println("2 - Playlist");
        System.out.println("3 - Músicas Favoritas");
        System.out.println("4 - Playlist Gerada Automaticamente");
        System.out.println("0 - Voltar");

        int option = inputHandler.readInt("-> ");
        switch (option) {
            case 1 -> entityCreator.registarReproducaoUnica();
            case 2 -> entityCreator.registarReproducaoPlaylist(false, false);
            case 3 -> entityCreator.registarReproducaoPlaylist(true, false);
            case 4 -> entityCreator.registarReproducaoPlaylist(false, true);
            case 0 -> { /* voltar */ }
            default -> System.out.println("Opção inválida.");
        }
    }

    /**
     * Saves the current state of the system to a specified directory.
     */
    private void guardarEstado() {
        String storeDir = inputHandler.readLine("Insira o local onde guardar o estado (ex: data/Estado1/): ");
        spotifum.guardarEstado(storeDir);
        System.out.println("Estado guardado com sucesso!");
    }

    /**
     * Loads the system state from a specified directory, warning about data overwrite.
     */
    private void carregarEstado() {
        String loadDir = inputHandler.readLine("Insira o local de onde carregar o estado (ex: data/Estado1/): ");
        System.out.println("Ao continuar vai apagar todas as alterações locais feitas até ao momento! Deseja continuar? (S - Continuar / N - Voltar)");
        if (!inputHandler.readBool()) {
            return;
        }
        spotifum.loadEstado(loadDir);
        System.out.println("Estado carregado com sucesso!");
    }

    /**
     * Displays various statistics such as most reproduced music, top listeners, and playlist info.
     */
    private void estatisticas() {
        try {
            Musica musicaMaisReproduzida = spotifum.executeQueryWhichIsTheMostReproducedMusic();
    
            String interpreteMaisEscutado = spotifum.executeQueryWhoIsTheMostListenedInterpreter();
    
            Utilizador utilizadorMaisMusicas;

            System.out.println("Deseja especificar um intervalo de tempo para o utilizador que mais músicas ouviu? (S/N)");
            boolean usarIntervalo = inputHandler.readBool();

            if (usarIntervalo) {
                String dataInicialStr = inputHandler.readLine("Data inicial (formato YYYY-MM-DDTHH:MM): ");
                String dataFinalStr = inputHandler.readLine("Data final (formato YYYY-MM-DDTHH:MM): ");
                try {
                    LocalDateTime dataInicial = LocalDateTime.parse(dataInicialStr);
                    LocalDateTime dataFinal = LocalDateTime.parse(dataFinalStr);
                    utilizadorMaisMusicas = spotifum.executeQueryUserThatListenedToMoreMusics(dataInicial, dataFinal);
                } catch (Exception e) {
                    System.out.println("Erro ao ler as datas. A mostrar resultado sem intervalo de tempo.");
                    utilizadorMaisMusicas = spotifum.executeQueryUserThatListenedToMoreMusics();
                }
            } else {
                utilizadorMaisMusicas = spotifum.executeQueryUserThatListenedToMoreMusics();
            }

            if (musicaMaisReproduzida != null) {
                System.out.println("[Música Mais Reproduzida] - " + musicaMaisReproduzida.getNome() + " - " +
                        musicaMaisReproduzida.getInterprete() + " - " +
                        musicaMaisReproduzida.getNumeroReproducoes() + " reproduções");
            } else {
                System.out.println("[Música Mais Reproduzida] - Nenhuma");
            }

            System.out.println("[Intérprete Mais Escutado] - " + (interpreteMaisEscutado != null ? interpreteMaisEscutado : "Nenhum"));
            System.out.println("[Utilizador que mais músicas ouviu] - " + (utilizadorMaisMusicas != null ? utilizadorMaisMusicas.getEmail() : "Nenhum"));
    
            Utilizador utilizadorMaisPontos = spotifum.executeQueryWhoIsTheUserWithTheMostPoints();
            System.out.println("[Utilizador com mais Pontos] - " + (utilizadorMaisPontos != null ? utilizadorMaisPontos.getEmail() : "Nenhum"));
    
            GeneroMusical generoMaisReproduzido = spotifum.executeQueryWhatGenreOfMusicIsTheMostPlayed();
            System.out.println("[Tipo de Música Mais Reproduzida] - " + (generoMaisReproduzido != null ? generoMaisReproduzido : "Nenhum"));
    
            int numPlaylistsPublicas = spotifum.executeQueryHowManyPublicPalylistsExist();
            System.out.println("[Número de Playlists Públicas] - " + numPlaylistsPublicas);
    
            Utilizador utilizadorMaisPlaylists = spotifum.executeQueryWhoIsTheUserWithTheMostPlaylists();
            System.out.println("[Utilizador com mais Playlists] - " + (utilizadorMaisPlaylists != null ? utilizadorMaisPlaylists.getEmail() : "Nenhum"));
        } catch (Exception e) {
            System.out.println("Erro ao gerar estatísticas: " + e.getMessage());
        }
    }

    /**
     * Generates or updates an automatic playlist for a given user based on preferences.
     */
    private void gerarPlaylist() {
        String email = inputHandler.readLine("Email do Utilizador: ");

        int maxDuration = inputHandler.readInt("Máximo de Duração da Playlist (em segundos, se não quiser ter a duração em conta insira 0): ");

        System.out.println("Deseja gerar uma playlist apenas com música explicita? (S - Sim / N - Não)");
        boolean explicit = inputHandler.readBool();

        Utilizador u = spotifum.getUser(email);

        // Ir às Reproduções ver as Músicas que o Utilizador ouve (contando repetidas por causa das preferências)
        List<Musica> allMusics = spotifum.getAllMusics();
        
        Playlist generated = spotifum.getPlaylist(email, "generated_musics");

        if (generated != null) {
            // Se a 'generated_musics' já existe, apenas atualiza
            spotifum.updateGeneratedMusicsPlaylist(u, allMusics, maxDuration, explicit);
            System.out.println("Playlist atualizada com sucesso de acordo com as tuas preferências!");
        } else {
            // Se a 'generated_musics' não existe, cria uma nova
            spotifum.createGeneratedMusicsPlaylist(u, allMusics, maxDuration, explicit);
            System.out.println("Playlist gerada com sucesso de acordo com as tuas preferências!");
        }
    }
}
