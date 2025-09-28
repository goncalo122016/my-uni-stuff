package spotifum.queries;

import java.io.Serializable;
import java.util.List;
import spotifum.Playlists.Playlist;
import spotifum.Playlists.PlaylistManager;

/**
 * Consulta que determina quantas playlists públicas existem no sistema.
 */
public class HowManyPublicPlaylistsExist implements Serializable {

    /**
     * Executa a query para contar as playlists públicas.
     *
     * @param managerPlaylist Gerenciador de playlists.
     * @return Número total de playlists públicas.
     */
    public int executeQuery(PlaylistManager managerPlaylist) {
        List<Playlist> playlists = managerPlaylist.getAllPlaylists();
        return (int) playlists.stream().filter(Playlist::isPublica).count();
    }
}
