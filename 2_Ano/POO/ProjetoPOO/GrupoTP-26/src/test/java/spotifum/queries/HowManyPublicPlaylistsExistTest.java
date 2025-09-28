package spotifum.queries;

import java.util.List;

import static org.junit.jupiter.api.Assertions.assertEquals;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;

import spotifum.Musics.MusicManager;
import spotifum.Musics.Musica;
import spotifum.Musics.implementations.GeneroMusical;
import spotifum.Musics.implementations.TipoMusica;
import spotifum.Playlists.Playlist;
import spotifum.Playlists.PlaylistManager;
import spotifum.Users.PlanoSubscricao;
import spotifum.Users.UserManager;
import spotifum.Users.Utilizador;

/**
 * The tests for the HowManyPublicPlaylistsExist class.
 *
 * This class contains tests for the constructors, methods, and operations of the HowManyPublicPlaylistsExist class,
 *
 * @author  Afonso Martins (a106931), Luís Felício (a106913) and Goncalo Castro (a107337)
 * @version (17052025)
 */
public class HowManyPublicPlaylistsExistTest {

    private PlaylistManager playlistManager;
    private UserManager userManager;
    private MusicManager musicManager;

     /**
     * Sets up the test environment.
     */
    @BeforeEach
    void setUp() {
        userManager = new UserManager();
        musicManager = new MusicManager();
        playlistManager = new PlaylistManager();

        Utilizador user1 = userManager.createUser("User One", "user1@example.com", "Sei lá", PlanoSubscricao.FREE);
        Utilizador user2 = userManager.createUser("User Two", "user2@example.com", "Sei lá 2", PlanoSubscricao.FREE);

        userManager.insertUser(user1);
        userManager.insertUser(user2);

        Musica m1 = musicManager.createMusica("Song1", "Artist1", "Editor1", "Lyrics1", List.of("Track1"), GeneroMusical.POP, 180, "Album1", TipoMusica.NORMAL);
        Musica m2 = musicManager.createMusica("Song2", "Artist2", "Editor2", "Lyrics2", List.of("Track2"), GeneroMusical.HIPHOP, 320, "Album2", TipoMusica.NORMAL);

        musicManager.insertMusica(m1);
        musicManager.insertMusica(m2);

        Playlist p1 = playlistManager.createPlaylist(userManager, musicManager, "user1@example.com", List.of("Song1"), "Chill Vibes", true);
        Playlist p2 = playlistManager.createPlaylist(userManager, musicManager, "user1@example.com", List.of("Song2"), "Workout Mix", false);
        Playlist p3 = playlistManager.createPlaylist(userManager, musicManager, "user2@example.com", List.of("Song1"), "Summer Hits", true);
        Playlist p4 = playlistManager.createPlaylist(userManager, musicManager, "user2@example.com", List.of("Song2"), "Evening Relax", false);

        playlistManager.insertPlaylist(p1);
        playlistManager.insertPlaylist(p2);
        playlistManager.insertPlaylist(p3);
        playlistManager.insertPlaylist(p4);
    }

    /**
    * Test the constructor of the HowManyPublicPlaylistsExist class.
    */
    @Test
    void testExecuteQueryWithMixedPlaylists() {
        HowManyPublicPlaylistsExist query = new HowManyPublicPlaylistsExist();
        int result = query.executeQuery(playlistManager);
        assertEquals(2, result); // 2 playlists públicas
    }

    /**
    * Test the constructor of the HowManyPublicPlaylistsExist class.
    */
    @Test
    void testExecuteQueryWithNoPublicPlaylists() {
        // Novo ambiente com playlists privadas apenas
        userManager = new UserManager();
        musicManager = new MusicManager();
        playlistManager = new PlaylistManager();

        Utilizador user3 = userManager.createUser("User Three", "user3@example.com", "Sei lá 3", PlanoSubscricao.FREE);
        userManager.insertUser(user3);

        Musica musica3 = musicManager.createMusica("Song3", "Artist3", "Editor3", "Lyrics3", List.of("Track3"), GeneroMusical.ROCK, 200, "Album3", TipoMusica.NORMAL);
        musicManager.insertMusica(musica3);

        Playlist p5 = playlistManager.createPlaylist(userManager, musicManager, "user3@example.com", List.of("Song3"), "Private Rock", false);
        Playlist p6 = playlistManager.createPlaylist(userManager, musicManager, "user3@example.com", List.of("Song3"), "More Private", false);

        playlistManager.insertPlaylist(p5);
        playlistManager.insertPlaylist(p6);

        HowManyPublicPlaylistsExist query = new HowManyPublicPlaylistsExist();
        int result = query.executeQuery(playlistManager);
        assertEquals(0, result);
    }

    /**
     * Test the constructor of the HowManyPublicPlaylistsExist class.
     */
    @Test
    void testExecuteQueryWithAllPublicPlaylists() {
        // Novo ambiente com todas públicas
        userManager = new UserManager();
        musicManager = new MusicManager();
        playlistManager = new PlaylistManager();

        Utilizador user4 = userManager.createUser("User Four", "user4@example.com", "password123", PlanoSubscricao.FREE);
        userManager.insertUser(user4);

        Musica musica4 =musicManager.createMusica("Song4", "Artist4", "Editor4", "Lyrics4", List.of("Track4"), GeneroMusical.CLASSICA, 240, "Album4", TipoMusica.NORMAL);
        musicManager.insertMusica(musica4);

        Playlist p7 =  playlistManager.createPlaylist(userManager, musicManager, "user4@example.com", List.of("Song4"), "Public A", true);
        Playlist p8 = playlistManager.createPlaylist(userManager, musicManager, "user4@example.com", List.of("Song4"), "Public B", true);
        playlistManager.insertPlaylist(p7);
        playlistManager.insertPlaylist(p8);

        HowManyPublicPlaylistsExist query = new HowManyPublicPlaylistsExist();
        int result = query.executeQuery(playlistManager);
        assertEquals(2, result);
    }
}
