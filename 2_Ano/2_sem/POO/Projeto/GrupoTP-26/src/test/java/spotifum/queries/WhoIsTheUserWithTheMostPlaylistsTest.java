package spotifum.queries;

import java.util.List;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertNull;
import static org.junit.jupiter.api.Assertions.assertTrue;
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
 * The tests for the WhoIsTheUserWithTheMostPlaylists class.
 *
 * This class contains tests for the constructors, methods, and operations of the WhoIsTheUserWithTheMostPlaylists class,
 *
 * @author  Afonso Martins (a106931), Luís Felício (a106913) and Goncalo Castro (a107337)
 * @version (17052025)
 */
public class WhoIsTheUserWithTheMostPlaylistsTest {

    private PlaylistManager playlistManager;
    private UserManager userManager;
    private MusicManager musicManager;

    /**
     * Sets up the test environment.
     */
    @BeforeEach
    void setUp() {
        playlistManager = new PlaylistManager();
        userManager = new UserManager();
        musicManager = new MusicManager();

        Utilizador u1 = userManager.createUser("User 1", "user1@example.com", "123", PlanoSubscricao.FREE);
        Utilizador u2 = userManager.createUser("User 2", "user2@example.com", "123", PlanoSubscricao.FREE);

        userManager.insertUser(u1);
        userManager.insertUser(u2);

        Musica m = musicManager.createMusica("Song", "Artist", "Editor", "Lyrics", List.of("Track"), GeneroMusical.POP, 180, "Album", TipoMusica.NORMAL);
        musicManager.insertMusica(m);

        // user1: 2 playlists
        Playlist p1 = playlistManager.createPlaylist(userManager, musicManager, "user1@example.com", List.of("Song"), "Playlist 1", true);
        Playlist p2 = playlistManager.createPlaylist(userManager, musicManager, "user1@example.com", List.of("Song"), "Playlist 2", true);
        playlistManager.insertPlaylist(p1);
        playlistManager.insertPlaylist(p2);

        // user2: 1 playlist
        Playlist p3 = playlistManager.createPlaylist(userManager, musicManager, "user2@example.com", List.of("Song"), "Playlist 3", true);
        playlistManager.insertPlaylist(p3);
    }

    /**
    * Test the constructor of the WhoIsTheUserWithTheMostPlaylists class.
    */
    @Test
    void testUserWithMostPlaylists() {
        WhoIsTheUserWithTheMostPlaylists query = new WhoIsTheUserWithTheMostPlaylists();
        Utilizador result = query.executeQuery(playlistManager, userManager);
        assertNotNull(result);
        assertEquals("user1@example.com", result.getEmail());
    }

    /**
    * Test the constructor of the WhoIsTheUserWithTheMostPlaylists class.
    */
    @Test
    void testTieReturnsAny() {
        playlistManager = new PlaylistManager();
        userManager = new UserManager();
        musicManager = new MusicManager();

        Utilizador u1 = userManager.createUser("User 1", "u1@example.com", "123", PlanoSubscricao.FREE);
        Utilizador u2 = userManager.createUser("User 2", "u2@example.com", "123", PlanoSubscricao.FREE);
        userManager.insertUser(u1);
        userManager.insertUser(u2);

        Musica m = musicManager.createMusica("Song", "Artist", "Editor", "Lyrics", List.of("Track"), GeneroMusical.POP, 180, "Album", TipoMusica.NORMAL);
        musicManager.insertMusica(m);

        Playlist p1 = playlistManager.createPlaylist(userManager, musicManager, "u1@example.com", List.of("Song"), "A", true);
        Playlist p2 = playlistManager.createPlaylist(userManager, musicManager, "u2@example.com", List.of("Song"), "B", true);
        playlistManager.insertPlaylist(p1);
        playlistManager.insertPlaylist(p2);

        WhoIsTheUserWithTheMostPlaylists query = new WhoIsTheUserWithTheMostPlaylists();
        Utilizador result = query.executeQuery(playlistManager, userManager);
        assertTrue(result.getEmail().equals("u1@example.com") || result.getEmail().equals("u2@example.com"));
    }

    /**
    * Test the constructor of the WhoIsTheUserWithTheMostPlaylists class.
    */
    @Test
    void testNoPlaylistsReturnsNull() {
        playlistManager = new PlaylistManager();
        userManager = new UserManager();

        Utilizador u = userManager.createUser("Alone", "alone@example.com", "123", PlanoSubscricao.FREE);
        userManager.insertUser(u);

        WhoIsTheUserWithTheMostPlaylists query = new WhoIsTheUserWithTheMostPlaylists();
        Utilizador result = query.executeQuery(playlistManager, userManager);
        assertNull(result);
    }
}
