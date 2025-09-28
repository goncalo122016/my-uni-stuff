package spotifum.Menu;

import java.time.LocalDateTime;
import java.util.List;
import spotifum.Musics.Musica;
import spotifum.Musics.implementations.GeneroMusical;
import spotifum.Musics.implementations.TipoMusica;
import spotifum.Playlists.Playlist;
import spotifum.Playlists.PlayManager;
import spotifum.Spotifum;
import spotifum.Users.PlanoSubscricao;

/**
 * Class responsible for creating entities in the system such as users,
 * musics, playlists and registering reproductions.
 */
public class EntityCreator {
    private final Spotifum spotifum;
    private final InputHandler inputHandler;

    /**
     * Constructs an EntityCreator with the given SystemManager and InputHandler.
     *
     * @param spotifum     The system manager.
     * @param inputHandler The input data handler.
     */
    public EntityCreator(Spotifum spotifum, InputHandler inputHandler) {
        this.spotifum = spotifum;
        this.inputHandler = inputHandler;
    }

    /**
     * Creates a new user by requesting the necessary information
     * and inserts it into the system.
     */
    public void criarUtilizador() {
        System.out.println("Insira as seguintes informações:");
        String nome = inputHandler.readLine("Nome: ");
        String email = inputHandler.readLine("Email: ");
        String morada = inputHandler.readLine("Morada: ");
        PlanoSubscricao sub = inputHandler.readPlanoSubscricao();

        try {
            spotifum.createUser(nome, email, morada, sub);

            // Criar a Playlist de 'Favorite_Musics'
            if (sub != PlanoSubscricao.FREE) {
                spotifum.createFavoriteMusicsPlaylist(spotifum.getUser(email));
            }
            System.out.println("Utilizador criado com sucesso!");
        } catch (RuntimeException e) {
            System.out.println("Erro ao criar utilizador: " + e.getMessage());
        }
    }

    /**
     * Creates a new music entity by requesting its details
     * and inserts it into the system.
     */
    public void criarMusica() {
        System.out.println("Insira as seguintes informações:");
        String nome = inputHandler.readLine("Nome: ");
        String intp = inputHandler.readLine("Intérprete: ");
        String editora = inputHandler.readLine("Editora: ");
        String letra = inputHandler.readLine("Letra: ");
        System.out.println("Partitura (prima 'Enter' numa linha em branco para terminar):");
        List<String> musica = inputHandler.readMuitasLinhas();
        GeneroMusical genero = inputHandler.readGeneroMusical();
        int duracao = inputHandler.readInt("Duração (em segundos): ");
        TipoMusica tipo = inputHandler.readTipoMusica();
        String urlVideo = "";
        if (tipo == TipoMusica.MULTIMEDIA) {
            urlVideo = inputHandler.readLine("URL do vídeo: ");
        }

        try {
            spotifum.createMusica(nome, intp, editora, letra, musica, genero, duracao, urlVideo, tipo);

            System.out.println("Música criada com sucesso!");

        } catch (RuntimeException e) {
            System.out.println("Erro ao criar a Música: " + e.getMessage());
        }
    }

    /**
     * Creates a new playlist for a specified user by
     * requesting playlist name, musics and privacy settings.
     */
    public void criarPlaylist() {
        System.out.println("Insira as seguintes informações:");
        String email = inputHandler.readLine("Email: ");

        // Verifica se o usuário existe
        if (!spotifum.existsUser(email)) {
            System.out.println("O utilizador não existe!");
            return;
        }

        String nome = inputHandler.readLine("Nome: ");
        System.out.println("Músicas: (prima 'Enter' numa linha em branco para terminar):");
        List<String> allMusicas = spotifum.getAllMusics().stream().map(Musica::getNome).toList();
        List<String> musicasList = inputHandler.readMuitasMusicas(allMusicas);

        System.out.print("Publica (S - sim / N - não): ");
        boolean publica = inputHandler.readBool();

        try {
            spotifum.createPlaylist(email, musicasList, nome, publica);

            System.out.println("Playlist criada com sucesso!");

        } catch (RuntimeException e) {
            System.out.println("Erro ao criar a Playlist: " + e.getMessage());
        }
    }

    /**
     * Registers a single music reproduction for a user,
     * updates playback stats, and optionally manages favorites.
     */
    public void registarReproducaoUnica() {
        String email = inputHandler.readLine("Introduza o email do Utilizador: ");
        if (!spotifum.existsUser(email)) {
            System.out.println("O Utilizador não existe!");
            return;
        }

        String nome = inputHandler.readLine("Introduza o nome da Música a reproduzir: ");
        try {
            if (!spotifum.existsMusic(nome)) {
                System.out.println("A Música não existe!");
                return;
            }

            spotifum.createReproducao(nome, email, LocalDateTime.now());
            Musica musica = spotifum.getMusica(nome);
            spotifum.addMusicReproduction(nome, email, LocalDateTime.now());
            inputHandler.displayMusicPlaying(musica);

            boolean isFavorite = spotifum.isMusicFavorite(email, nome);
            System.out.println("\nDeseja " + (isFavorite ? "remover" : "adicionar") + " esta música " +
                    (isFavorite ? "dos" : "aos") + " favoritos?");

            if (inputHandler.readBool()) {
                if (isFavorite) {
                    // Remove da playlist de favoritos
                    spotifum.removeFavoriteMusic(nome, email);

                    spotifum.removeFavoriteMusicPlaylist(nome, email);

                    System.out.println("Música removida dos favoritos com sucesso!");
                } else {
                    // Adiciona à playlist de favoritos
                    spotifum.addFavoriteMusic(nome, email);

                    spotifum.addFavoriteMusicPlaylist(nome, email);

                    System.out.println("Música adicionada aos favoritos com sucesso!");
                }
            }
        } catch (RuntimeException e) {
            System.out.println("Erro ao reproduzir música: " + e.getMessage());
        }
    }

    /**
     * Registers the reproduction of a playlist for a user,
     * including special handling for favorites or generated playlists.
     *
     * @param wantsFavorites true if the favorites playlist should be played
     * @param wantsGenerated true if the generated playlist should be played
     */
    public void registarReproducaoPlaylist(boolean wantsFavorites, boolean wantsGenerated) {
        String email = inputHandler.readLine("Introduza o email do Utilizador: ");
        if (!spotifum.existsUser(email)) {
            System.out.println("O Utilizador não existe!");
            return;
        }

        String nome = wantsFavorites ? "favorite_musics"
                : (wantsGenerated ? "generated_musics"
                        : inputHandler.readLine("Introduza o nome da Playlist a reproduzir: "));
        try {
            if (!spotifum.existsPlaylist(email, nome)) {
                System.out.println("A Playlist não existe!");
                return;
            }

            Playlist p = spotifum.getPlaylist(email, nome);
            List<Musica> musicas = p.getMusicas();
            if (musicas.isEmpty()) {
                System.out.println("A playlist está vazia!");
                return;
            }

            PlayManager playManager = spotifum.createPlayManager();

            while (playManager.isPlaying()) {
                if (playManager.needsReproduction()) {
                    Musica mAtual = playManager.reproduzirMusicaAtual(musicas, email);
                    spotifum.addMusicReproduction(mAtual.getNome(), email, LocalDateTime.now());
                    inputHandler.displayMusicPlaying(mAtual);
                }

                mostrarControlosReproducao(playManager, email);

                int option = inputHandler.readInt("-> ");
                playManager.processarComando(option, musicas, email);
            }
            spotifum.incrementNReproducoes(email, nome);
        } catch (RuntimeException e) {
            System.out.println("Erro ao reproduzir playlist: " + e.getMessage());
        }
    }

    /**
     * Displays playback controls for the current playing session.
     *
     * @param playManager The PlayManager handling playback.
     * @param email The email of the user currently playing music.
     */
    private void mostrarControlosReproducao(PlayManager playManager, String email) {
        System.out.println("\nControlos de Reprodução:");
        System.out.println("1 - Próxima música");
        System.out.println("2 - Música anterior");
        System.out.println("3 - Repetir música atual");

        Musica musicaAtual = playManager.getMusicaAtual();
        boolean isFavorite = spotifum.isMusicFavorite(email, musicaAtual.getNome());
        System.out.println("4 - " + (isFavorite ? "Remover dos favoritos" : "Adicionar aos favoritos"));
        System.out.println("0 - Parar reprodução");
    }
}
