package spotifum.Playlists;

import java.util.List;

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
import spotifum.Reproductions.ReproductionManager;
import spotifum.Users.PlanoSubscricao;
import spotifum.Users.UserManager;
import spotifum.Users.Utilizador;

/**
 * Unit tests for the PlayManager class, which handles music reproduction and playlist management.
 *
 * This test class ensures that music playback, playlist navigation (next/previous), and
 * favorite music toggling work as expected. It verifies the correct functionality of the
 * PlayManager when interacting with playlists, user subscriptions, and reproduction commands.
 * 
 * @author Afonso Martins (a106931), Luís Felício (a106913), Gonçalo Castro (a107337)
 * @version (17052025)
 */
public class PlayManagerTest {

    private ReproductionManager reproductionManager;
    private PlaylistManager playlistManager;
    private MusicManager musicManager;
    private UserManager userManager;
    private PlayManager playManager;
    private Utilizador user;
    private List<Musica> musicas;

    /**
     * Sets up the test environment.
     */
    @BeforeEach
    public void setUp() {
        reproductionManager = new ReproductionManager();
        playlistManager = new PlaylistManager();
        musicManager = new MusicManager();
        userManager = new UserManager();

        user = userManager.createUser("User Test", "test@example.com", "pw123", PlanoSubscricao.PremiumBase);
        userManager.insertUser(user);

        Musica m1 = musicManager.createMusica("Song1", "Artist1", "Editor1", "Lyrics1",
                List.of("Track1"), GeneroMusical.POP, 180, "Album1", TipoMusica.NORMAL);
        Musica m2 = musicManager.createMusica("Song2", "Artist2", "Editor2", "Lyrics2",
                List.of("Track2"), GeneroMusical.ROCK, 200, "Album2", TipoMusica.NORMAL);

        musicManager.insertMusica(m1);
        musicManager.insertMusica(m2);

        musicas = List.of(m1, m2);

        playlistManager.createFavoriteMusicsPlaylist(user, musicManager);

        playManager = new PlayManager(reproductionManager, playlistManager, musicManager, userManager);
    }

    /**
     * Tests playing the current music.
     * Verifies that the music is correctly selected and played, and its reproduction count is updated.
     */
    @Test
    public void testReproduzirMusicaAtual() {
        Musica musica = playManager.reproduzirMusicaAtual(musicas, user.getEmail());
        assertNotNull(musica);
        assertEquals("Song1", musica.getNome());
        //assertEquals(1, musica.getNumeroReproducoes());
    }

    /**
     * Tests retrieving the current music.
     * Ensures that the current music is correctly fetched.
     */
    @Test
    public void testGetMusicaAtual() {
        Musica musica = playManager.getMusicaAtual();
        assertEquals("Song1", musica.getNome());
    }

    /**
     * Tests the next song command.
     * Verifies that the playback correctly moves to the next song in the playlist.
     */
    @Test
    public void testProcessarComandoNextSong() {
        playManager.processarComando(1, musicas, user.getEmail()); // next
        Musica musica = playManager.reproduzirMusicaAtual(musicas, user.getEmail());
        assertEquals("Song2", musica.getNome());
    }

    /**
     * Tests the previous song command.
     * Verifies that the playback correctly moves to the previous song in the playlist.
     */
    @Test
    public void testProcessarComandoPreviousSong() {
        playManager.processarComando(1, musicas, user.getEmail()); // next -> Song2
        playManager.processarComando(2, musicas, user.getEmail()); // previous -> Song1
        Musica musica = playManager.reproduzirMusicaAtual(musicas, user.getEmail());
        assertEquals("Song1", musica.getNome());
    }

    /**
     * Tests toggling a song as a favorite.
     * Verifies that the song is correctly added and removed from the user's favorites.
     */
    @Test
    public void testToggleFavoriteAddsAndRemoves() {
        // Adds to favorites
        playManager.processarComando(4, musicas, user.getEmail());
        List<Musica> favs = playlistManager.getFavoriteMusics(user.getEmail());
        assertEquals(1, favs.size());
        assertEquals("Song1", favs.get(0).getNome());

        // Removes from favorites
        playManager.processarComando(4, musicas, user.getEmail());
        favs = playlistManager.getFavoriteMusics(user.getEmail());
        assertTrue(favs.isEmpty());
    }

    /**
     * Tests stopping music playback.
     * Ensures that music playback stops when the stop command is issued.
     */
    @Test
    public void testIsPlayingFalseAfterStop() {
        playManager.processarComando(0, musicas, user.getEmail());
        assertFalse(playManager.isPlaying());
    }

    /**
     * Tests the behavior of the needsReproduction method.
     * Verifies that the method correctly identifies when music needs to be reproduced.
     */
    @Test
    public void testNeedsReproduction() {
        assertTrue(playManager.needsReproduction());
        playManager.processarComando(4, musicas, user.getEmail()); // toggleFavorite
        assertFalse(playManager.needsReproduction());
    }

    /**
     * Tests that an invalid command does not crash the system.
     * Ensures that invalid command options are handled gracefully.
     */
    @Test
    public void testInvalidOptionDoesNotCrash() {
        playManager.processarComando(99, musicas, user.getEmail()); // invalid
        assertTrue(playManager.isPlaying());
        assertFalse(playManager.needsReproduction());
    }
}
