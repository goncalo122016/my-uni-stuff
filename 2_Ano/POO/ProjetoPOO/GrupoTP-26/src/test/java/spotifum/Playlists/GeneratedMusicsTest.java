package spotifum.Playlists;

import java.util.List;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertThrows;
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
 * Test class for the GeneratedMusics class.
 *
 * This class contains tests for creating a user's favorite music playlist based
 * on their preferred genres, and ensuring that only premium users can create
 * such playlists.
 *
 * @author  Afonso Martins (a106931), Luís Felício (a106913), and Goncalo Castro (a107337)
 * @version (17052025)
 */
public class GeneratedMusicsTest {

    private UserManager userManager;
    private MusicManager musicManager;
    private List<Musica> musicas;

    /**
     * Sets up the test environment.
     */
    @BeforeEach
    void setUp() {
        userManager = new UserManager();
        musicManager = new MusicManager();

        Musica m1 = musicManager.createMusica("Song1", "Artist1", "Editor1", "Lyrics1", List.of("Track1"), GeneroMusical.POP, 180, "Album1", TipoMusica.MULTIMEDIA);
        Musica m2 = musicManager.createMusica("Song2", "Artist2", "Editor2", "Lyrics2", List.of("Track2"), GeneroMusical.ROCK, 200, "Album2", TipoMusica.NORMAL);
        Musica m3 = musicManager.createMusica("Song3", "Artist3", "Editor3", "Lyrics3", List.of("Track3"), GeneroMusical.JAZZ, 240, "Album3", TipoMusica.EXPLICITA);

        musicManager.insertMusica(m1);
        musicManager.insertMusica(m2);
        musicManager.insertMusica(m3);

        musicas = List.of(m1, m2, m3);
    }

    /**
     * Tests the generation of a music playlist for a Premium user.
     * Verifies that only the favorite genres of the user are included in the playlist.
     */
    @Test
    void testGeneratedMusicsForPremiumUser() {
        Utilizador premiumUser = userManager.createUser("Premium User", "premium@example.com", "password123", PlanoSubscricao.PremiumTop);

        premiumUser.addMusicToFavorites(musicas.get(0)); // Song1 (POP)
        premiumUser.addMusicToFavorites(musicas.get(1)); // Song2 (ROCK)
        //List<GeneroMusical> preferredGenres = premiumUser.getPreferredGenres();

        GeneratedMusics generatedPlaylist = new GeneratedMusics(musicas, premiumUser, 0, false);

        List<Musica> playlistMusics = generatedPlaylist.getMusicas();
        assertEquals(2, playlistMusics.size()); // POP e ROCK
        assertEquals("Song1", playlistMusics.get(0).getNome());
        assertEquals("Song2", playlistMusics.get(1).getNome());
    }

    /**
     * Tests that a non-premium user cannot generate a music playlist.
     * Verifies that an exception is thrown when a Free user tries to create a playlist.
     */
    @Test
    void testGeneratedMusicsForNonPremiumUser() {
        Utilizador nonPremiumUser = userManager.createUser("Non-Premium User", "nonpremium@example.com", "password123", PlanoSubscricao.FREE);

        nonPremiumUser.addMusicToFavorites(musicas.get(0)); // Song1 (POP)
        nonPremiumUser.addMusicToFavorites(musicas.get(1)); // Song2 (ROCK)


        IllegalArgumentException exception = assertThrows(IllegalArgumentException.class, () -> new GeneratedMusics(musicas, nonPremiumUser, 0, false));
        assertEquals("Only premium users can have generated music playlists.", exception.getMessage());
    }

    /**
     * Tests that a Premium user can update their generated music playlist.
     * Verifies that adding new songs to the favorites correctly updates the playlist.
     */
    @Test
    void testUpdateGeneratedMusicsForPremiumUser() {
        Utilizador premiumUser = userManager.createUser("Premium User", "premium@example.com", "password123", PlanoSubscricao.PremiumTop);

        premiumUser.addMusicToFavorites(musicas.get(0)); // Song1 (POP)
        premiumUser.addMusicToFavorites(musicas.get(2)); // Song3 (JAZZ)

        GeneratedMusics generatedPlaylist = new GeneratedMusics(musicas, premiumUser, 0, false);

        List<Musica> initialMusics = generatedPlaylist.getMusicas();
        assertEquals(2, initialMusics.size()); // POP e JAZZ
        assertEquals("Song1", initialMusics.get(0).getNome());
        assertEquals("Song3", initialMusics.get(1).getNome());

        premiumUser.addMusicToFavorites(musicas.get(1)); // Song2 (ROCK)

        generatedPlaylist.updateGeneratedMusics(musicas, premiumUser);

        List<Musica> updatedMusics = generatedPlaylist.getMusicas();
        assertEquals(3, updatedMusics.size()); // POP, JAZZ e ROCK
        assertEquals("Song1", updatedMusics.get(0).getNome());
        assertEquals("Song2", updatedMusics.get(1).getNome());
        assertEquals("Song3", updatedMusics.get(2).getNome());
    }
}
