package spotifum.Playlists;

import java.io.IOException;
import java.util.List;
import java.util.Map;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertTrue;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;

import spotifum.Musics.MusicManager;
import spotifum.Musics.Musica;
import spotifum.Musics.implementations.GeneroMusical;
import spotifum.Musics.implementations.TipoMusica;
import spotifum.Users.PlanoSubscricao;
import spotifum.Users.UserManager;
import spotifum.Users.Utilizador;

/**
 * Test class for the PlaylistManager class.
 *
 * This class contains various unit tests for managing playlists, including
 * creating, modifying, and deleting playlists. It also ensures that users can 
 * manage their favorite music playlists and tracks.
 *
 * @author  Afonso Martins (a106931), Luís Felício (a106913), and Goncalo Castro (a107337)
 * @version (17052025)
 */
public class PlaylistManagerTest {

    private PlaylistManager playlistManager;
    private UserManager userManager;
    private MusicManager musicManager;
    private Utilizador user;

    /**
     * Sets up the environment before each test.
     * Creates user, music instances, and populates the music list.
     */
    @BeforeEach
    public void setUp() {
        playlistManager = new PlaylistManager();
        userManager = new UserManager();
        musicManager = new MusicManager();

        user = userManager.createUser("User Test", "test@example.com", "pw123", PlanoSubscricao.PremiumBase);
        userManager.insertUser(user);

        Musica m1 = musicManager.createMusica("Song1", "Artist1", "Editor1", "Lyrics1", List.of("Track1"), GeneroMusical.POP, 180, "Album1", TipoMusica.NORMAL);
        Musica m2 = musicManager.createMusica("Song2", "Artist2", "Editor2", "Lyrics2", List.of("Track2"), GeneroMusical.HIPHOP, 320, "Album2", TipoMusica.NORMAL);
        Musica m3 = musicManager.createMusica("Song3", "Artist3", "Editor3", "Lyrics3", List.of("Track3"), GeneroMusical.ELETRONICA, 320, "Album3", TipoMusica.NORMAL);

        musicManager.insertMusica(m1);
        musicManager.insertMusica(m2);
        musicManager.insertMusica(m3);
    }

    /**
     * Tests the creation and insertion of a playlist.
     * Verifies that the playlist is created correctly and can be fetched.
     */
    @Test
    public void testCreateAndInsertPlaylist() {
        Playlist playlist = playlistManager.createPlaylist(userManager, musicManager,
                "test@example.com", List.of("Song1", "Song2"), "My Playlist", true);

        assertNotNull(playlist);
        assertEquals("My Playlist", playlist.getNome());
        assertEquals(2, playlist.getMusicas().size());

        playlistManager.insertPlaylist(playlist);

        Playlist fetched = playlistManager.getPlaylist("test@example.com", "My Playlist");
        assertNotNull(fetched);
        assertEquals("My Playlist", fetched.getNome());
    }

    /**
     * Tests retrieving all playlists for a specific user.
     * Verifies that all playlists for the user can be fetched correctly.
     */
    @Test
    public void testGetAllPlaylistsForUser() {
        Playlist playlist = playlistManager.createPlaylist(userManager, musicManager,
                "test@example.com", List.of("Song1"), "My Playlist", true);
        playlistManager.insertPlaylist(playlist);

        Map<String, Playlist> all = playlistManager.getAllPlaylistUser("test@example.com");
        assertNotNull(all);
        assertEquals(1, all.size());
        assertTrue(all.containsKey("my playlist"));
    }

    /**
     * Tests the creation of a public playlist.
     * Verifies that a public playlist can be created and fetched correctly.
     */
    @Test
    public void testCreatePublicPlaylist() {
        Playlist playlist = playlistManager.createPlaylist(userManager, musicManager,
                "test@example.com", List.of("Song1", "Song2"), "Public Playlist", true);
        playlistManager.insertPlaylist(playlist);

        Playlist fetched = playlistManager.getPlaylist("test@example.com", "Public Playlist");
        assertNotNull(fetched);
        assertTrue(fetched.isPublica());
        assertEquals("Public Playlist", fetched.getNome());
        assertEquals(2, fetched.getMusicas().size());
    }

    /**
     * Tests incrementing the number of reproductions for a playlist.
     * Verifies that the reproduction count is correctly incremented.
     */
    @Test
    public void testIncrementPlaylistReproductions() {
        Playlist playlist = playlistManager.createPlaylist(userManager, musicManager,
                "test@example.com", List.of("Song1", "Song2"), "My Playlist", false);
        playlistManager.insertPlaylist(playlist);

        Playlist fetched = playlistManager.getPlaylist("test@example.com", "My Playlist");
        int initialReproductions = fetched.getNumeroReproducoes();

        playlistManager.incrementNReproducoes("test@example.com", "My Playlist");
        fetched = playlistManager.getPlaylist("test@example.com", "My Playlist");
        assertEquals(initialReproductions + 1, fetched.getNumeroReproducoes());
    }

    /**
     * Tests saving and loading playlists to and from a file.
     * Verifies that playlists are saved and loaded correctly.
     */
    @Test
    public void testSaveAndLoadPlaylists() throws IOException, ClassNotFoundException {
        // Criar e salvar uma playlist
        Playlist playlist = playlistManager.createPlaylist(userManager, musicManager,
                "test@example.com", List.of("Song1", "Song2"), "My Playlist", true);
        playlistManager.insertPlaylist(playlist);

        String filePath = "playlists.dat";
        playlistManager.save(filePath);

        // Carregar e verificar se a playlist foi carregada corretamente
        PlaylistManager newManager = new PlaylistManager();
        newManager.load(filePath);

        Playlist loadedPlaylist = newManager.getPlaylist("test@example.com", "My Playlist");
        assertNotNull(loadedPlaylist);
        assertEquals("My Playlist", loadedPlaylist.getNome());
        assertEquals(2, loadedPlaylist.getMusicas().size());
    }

    /**
     * Tests removing a music track from all playlists.
     * Verifies that the music is removed from every playlist.
     */
    @Test
    public void testRemoveMusicFromAllPlaylists() {
        Playlist playlist = playlistManager.createPlaylist(userManager, musicManager,
                "test@example.com", List.of("Song1", "Song2"), "My Playlist", true);
        playlistManager.insertPlaylist(playlist);

        Playlist publicPlaylist = playlistManager.createPlaylist(userManager, musicManager,
                "test@example.com", List.of("Song1"), "Public Playlist", true);
        playlistManager.insertPlaylist(publicPlaylist);

        playlistManager.removeMusicFromAllPlaylists("Song1");

        // Verificar que a música foi removida de todas as playlists
        Playlist fetchedPlaylist = playlistManager.getPlaylist("test@example.com", "My Playlist");
        assertFalse(fetchedPlaylist.getMusicas().stream().anyMatch(m -> m.getNome().equals("Song1")));

        Playlist fetchedPublicPlaylist = playlistManager.getPlaylist("test@example.com", "Public Playlist");
        assertFalse(fetchedPublicPlaylist.getMusicas().stream().anyMatch(m -> m.getNome().equals("Song1")));
    }

    /**
     * Tests checking if a playlist exists for a user.
     * Verifies that the existence check works as expected.
     */
    @Test
    public void testExistsPlaylist() {
        Playlist playlist = playlistManager.createPlaylist(userManager, musicManager,
                "test@example.com", List.of("Song1", "Song2"), "My Playlist", true);
        playlistManager.insertPlaylist(playlist);

        assertTrue(playlistManager.existsPlaylist("test@example.com", "My Playlist"));
        assertFalse(playlistManager.existsPlaylist("test@example.com", "NonExistentPlaylist"));
    }
}
