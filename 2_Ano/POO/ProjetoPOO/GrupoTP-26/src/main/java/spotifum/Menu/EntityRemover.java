package spotifum.Menu;

import spotifum.Spotifum;
import spotifum.Musics.Musica;
import spotifum.Users.Utilizador;

import java.util.List;

/**
 * Class responsible for removing entities from the system.
 */
public class EntityRemover {
    private final Spotifum spotifum;
    private final InputHandler inputHandler;

    /**
     * Constructs an EntityRemover with the specified SystemManager and InputHandler.
     *
     * @param spotifum The spotifum application manager.
     * @param inputHandler The input data handler.
     */
    public EntityRemover(Spotifum spotifum, InputHandler inputHandler) {
        this.spotifum = spotifum;
        this.inputHandler = inputHandler;
    }

    /**
     * Removes a user from the system by email.
     */
    public void apagarUtilizador() {
        String email = inputHandler.readLine("Email: ");
        spotifum.removeUser(email);
        spotifum.removeAllPlaylistsFromUser(email);
        System.out.println("Utilizador apagado com sucesso!");
    }

    /**
     * Removes a music from the system, including removal from all playlists
     * and users' favorites.
     */
    public void apagarMusica() {
        String nome = inputHandler.readLine("Nome: ");
        try {
            if (!spotifum.existsMusic(nome)) {
                System.out.println("A música não existe!");
                return;
            }

            Musica musica = spotifum.getMusica(nome);

            // Remove a música de todas as playlists
            spotifum.removeMusicFromAllPlaylists(nome);
        
            // Remove a música de todos os usuários que a tenham como favorita
            List<Utilizador> usuarios = spotifum.getAllUsers();
            for (Utilizador user : usuarios) {
                try {
                    user.removeMusicFromFavorites(musica);
                } catch (RuntimeException ignored) {
                    // Ignora a exceção se a música não estiver nos favoritos do usuário
                }
            }

            // Remove a música do Manager de músicas
            spotifum.removeMusic(nome);
        
            System.out.println("Música apagada com sucesso!");
        } catch (RuntimeException e) {
            System.out.println("Erro ao apagar música: " + e.getMessage());
        }
    }

    /**
     * Removes a playlist.
     */
    public void apagarPlaylist() {
        String email = inputHandler.readLine("Email do Utilizador: ");

        if (!spotifum.existsUser(email)) {
            System.out.println("Erro: O utilizador não existe!");
            return;
        }
        
        String nome = inputHandler.readLine("Nome da Playlist: ");

        try {
            spotifum.removePlaylist(email, nome);
            System.out.println("Playlist apagada com sucesso!");
        } catch (RuntimeException e) {
            System.out.println("Erro: " + e.getMessage());
        }
    }
}