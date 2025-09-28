package spotifum.Playlists;

import java.time.LocalDateTime;
import java.util.List;
import spotifum.Musics.Musica;
import spotifum.Musics.MusicManager;
import spotifum.Reproductions.ReproductionManager;
import spotifum.Users.UserManager;

/**
 * Classe responsável pela gestão da reprodução de músicas dentro de uma playlist.
 * Controla o índice atual da música, o estado de reprodução e operações relacionadas,
 * como avançar, retroceder, repetir e alternar o status de favorito de uma música.
 */
public class PlayManager {
    private final ReproductionManager reproductionManager;
    private final PlaylistManager playlistManager;
    private final MusicManager musicManager;
    private final UserManager userManager;
    private int currentIndex;
    private boolean isPlaying;
    private boolean needsReproduction = true;

    /**
     * Construtor da classe PlayManager.
     *
     * @param reproductionManager Gerenciador de reproduções.
     * @param playlistManager Gerenciador de playlists.
     * @param musicManager Gerenciador de músicas.
     * @param userManager Gerenciador de utilizadores.
     */
    public PlayManager(ReproductionManager reproductionManager,
                       PlaylistManager playlistManager,
                       MusicManager musicManager,
                       UserManager userManager) {
        this.reproductionManager = reproductionManager;
        this.playlistManager = playlistManager;
        this.musicManager = musicManager;
        this.userManager = userManager;
        this.currentIndex = 0;
        this.isPlaying = true;
    }

    /**
     * Reproduz a música atual da lista fornecida, registra a reprodução e marca a música como tocada.
     *
     * @param musicas Lista de músicas da playlist.
     * @param email Email do utilizador que está a reproduzir a música.
     * @return A música atualmente reproduzida, ou null se o índice estiver fora dos limites.
     */
    public Musica reproduzirMusicaAtual(List<Musica> musicas, String email) {
        if (currentIndex >= 0 && currentIndex < musicas.size()) {
            Musica currentMusic = musicas.get(currentIndex);
            reproductionManager.createReproducao(currentMusic.getNome(), email, LocalDateTime.now());
            return currentMusic;
        }
        return null;
    }

    /**
     * Obtém a música atualmente selecionada no índice atual.
     *
     * @return A música atual, ou null se o índice estiver fora dos limites.
     */
    public Musica getMusicaAtual() {
        if (currentIndex >= 0 && currentIndex < musicManager.getAllMusics().size()) {
            return musicManager.getAllMusics().get(currentIndex);
        }
        return null;
    }

    /**
     * Alterna o estado de favorito da música para o utilizador especificado.
     * Se a música estiver nos favoritos, remove-a; caso contrário, adiciona-a.
     *
     * @param musica Música cujo estado de favorito será alternado.
     * @param userEmail Email do utilizador.
     */
    private void toggleFavorite(Musica musica, String userEmail) {
        try {
            if (playlistManager.isMusicFavorite(userEmail, musica.getNome())) {
                removeFromFavorites(musica, userEmail);
            } else {
                addToFavorites(musica, userEmail);
            }
        } catch (RuntimeException e) {
            System.out.println("Erro ao gerenciar favoritos: " + e.getMessage());
        }
    }

    /**
     * Indica se a reprodução de uma nova música é necessária.
     *
     * @return true se precisa reproduzir nova música, false caso contrário.
     */
    public boolean needsReproduction() {
        return needsReproduction;
    }

    /**
     * Processa o comando recebido do utilizador para controlar a reprodução.
     * Pode avançar, retroceder, repetir, alternar favoritos ou parar a reprodução.
     *
     * @param option Opção selecionada pelo utilizador.
     * @param musicas Lista de músicas da playlist atual.
     * @param email Email do utilizador.
     */
    public void processarComando(int option, List<Musica> musicas, String email) {
        switch (option) {
            case 1 -> {
                nextSong(musicas);
                needsReproduction = true;
            }
            case 2 -> {
                previousSong(musicas);
                needsReproduction = true;
            }
            case 3 -> needsReproduction = true;
            case 4 -> {
                toggleFavorite(musicas.get(currentIndex), email);
                needsReproduction = false;
            }
            case 0 -> isPlaying = false;
            default -> {
                System.out.println("Opção inválida.");
                needsReproduction = false;
            }
        }
    }

    /**
     * Avança para a próxima música na lista, retornando ao início se estiver no fim.
     *
     * @param musicas Lista de músicas.
     */
    private void nextSong(List<Musica> musicas) {
        currentIndex = (currentIndex + 1) % musicas.size();
    }

    /**
     * Retrocede para a música anterior na lista, indo para o fim se estiver no início.
     *
     * @param musicas Lista de músicas.
     */
    private void previousSong(List<Musica> musicas) {
        currentIndex = (currentIndex - 1 + musicas.size()) % musicas.size();
    }

    /**
     * Indica se a reprodução está em curso.
     *
     * @return true se estiver a reproduzir, false caso contrário.
     */
    public boolean isPlaying() {
        return isPlaying;
    }

    /**
     * Adiciona a música aos favoritos do utilizador e à playlist de favoritos.
     *
     * @param musica Música a adicionar.
     * @param email Email do utilizador.
     */
    private void addToFavorites(Musica musica, String email) {
        try {
            // Adiciona à playlist de favoritos
            playlistManager.addFavoriteMusic(musica.getNome(), email,  musicManager);
            
            // Adiciona à lista de favoritos do utilizador
            var user = userManager.getUser(email);
            user.addMusicToFavorites(musica);
            System.out.println("Música adicionada aos favoritos com sucesso!");
        } catch (RuntimeException e) {
            System.out.println("Erro ao adicionar música aos favoritos: " + e.getMessage());
        }
    }

    /**
     * Remove a música dos favoritos do utilizador e da playlist de favoritos.
     *
     * @param musica Música a remover.
     * @param email Email do utilizador.
     */
    private void removeFromFavorites(Musica musica, String email) {
        try {
            // Remove da playlist de favoritos
            playlistManager.removeFavoriteMusic(musica.getNome(), email);
            
            // Remove da lista de favoritos do utilizador
            var user = userManager.getUser(email);
            user.removeMusicFromFavorites(musica);
            System.out.println("Música removida dos favoritos com sucesso!");
        } catch (RuntimeException e) {
            System.out.println("Erro ao remover música dos favoritos: " + e.getMessage());
        }
    }
}
