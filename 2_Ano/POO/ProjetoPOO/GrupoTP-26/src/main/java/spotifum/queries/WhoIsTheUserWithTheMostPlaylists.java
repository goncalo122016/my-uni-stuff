package spotifum.queries;

import java.io.Serializable;
import java.util.Comparator;
import java.util.Map;
import spotifum.Playlists.PlaylistManager;
import spotifum.Users.UserManager;
import spotifum.Users.Utilizador;

/**
 * Consulta que retorna o utilizador com o maior número de playlists criadas.
 */
public class WhoIsTheUserWithTheMostPlaylists implements Serializable {

    /**
     * Executa a query para identificar o utilizador com mais playlists.
     *
     * @param managerPlaylist Gerenciador de playlists.
     * @param managerUser Gerenciador de utilizadores.
     * @return Utilizador com maior número de playlists, ou null se não houver.
     */
    public Utilizador executeQuery(PlaylistManager managerPlaylist, UserManager managerUser) {
        String emailMax = managerPlaylist.getPlaylists().entrySet().stream()
                .max(Comparator.comparingInt(entry -> entry.getValue().size()))
                .map(Map.Entry::getKey)
                .orElse(null);

        return emailMax == null ? null : managerUser.getUser(emailMax);
    }
}
