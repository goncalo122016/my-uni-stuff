package spotifum.queries;

import java.util.List;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.junit.jupiter.api.Assertions.assertTrue;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;

import spotifum.Musics.MusicManager;
import spotifum.Musics.Musica;
import spotifum.Musics.implementations.GeneroMusical;
import spotifum.Musics.implementations.TipoMusica;

/**
 * The tests for the WhoIsTheMostListenedInterpreter class.
 *
 * This class contains tests for the constructors, methods, and operations of the WhoIsTheMostListenedInterpreter class,
 *
 * @author  Afonso Martins (a106931), Luís Felício (a106913) and Goncalo Castro (a107337)
 * @version (17052025)
 */
public class WhoIsTheMostListenedInterpreterTest {

    private MusicManager musicManager;

    /**
     * Sets up the test environment.
     */
    @BeforeEach
    void setUp() {
        musicManager = new MusicManager();

        Musica m1 = musicManager.createMusica("Hit1", "Artist A", "Editor1", "Lyrics1", List.of("Track1"), GeneroMusical.POP, 180, "Album1", TipoMusica.NORMAL);
        Musica m2 = musicManager.createMusica("Hit2", "Artist B", "Editor2", "Lyrics2", List.of("Track2"), GeneroMusical.ROCK, 200, "Album2", TipoMusica.NORMAL);
        Musica m3 = musicManager.createMusica("Hit3", "Artist A", "Editor1", "Lyrics3", List.of("Track3"), GeneroMusical.JAZZ, 210, "Album3", TipoMusica.NORMAL);

        // Reproduções: Artist A = 5 (2 + 3), Artist B = 1
        m1.reproduzir();
        m1.reproduzir();

        m2.reproduzir();

        m3.reproduzir();
        m3.reproduzir();
        m3.reproduzir();

        musicManager.insertMusica(m1);
        musicManager.insertMusica(m2);
        musicManager.insertMusica(m3);
    }

    /**
    * Test the constructor of the WhoIsTheMostListenedInterpreter class.
    */
    @Test
    void testMostListenedInterpreterIsCorrect() {
        WhoIsTheMostListenedInterpreter query = new WhoIsTheMostListenedInterpreter();
        String result = query.executeQuery(musicManager);
        assertEquals("Artist A", result);
    }

    /**
    * Test the constructor of the WhoIsTheMostListenedInterpreter class.
    */
    @Test
    void testTieReturnsOneOfThem() {
        musicManager = new MusicManager();

        Musica m1 = musicManager.createMusica("Tie1", "Artist X", "Editor", "Lyrics", List.of("Track"), GeneroMusical.POP, 180, "Album", TipoMusica.NORMAL);
        Musica m2 = musicManager.createMusica("Tie2", "Artist Y", "Editor", "Lyrics", List.of("Track"), GeneroMusical.POP, 180, "Album", TipoMusica.NORMAL);

        m1.reproduzir();
        m1.reproduzir();

        m2.reproduzir();
        m2.reproduzir();

        musicManager.insertMusica(m1);
        musicManager.insertMusica(m2);

        WhoIsTheMostListenedInterpreter query = new WhoIsTheMostListenedInterpreter();
        String result = query.executeQuery(musicManager);
        assertTrue(result.equals("Artist X") || result.equals("Artist Y"));
    }

    /**
    * Test the constructor of the WhoIsTheMostListenedInterpreter class.
    */
    @Test
    void testEmptyMusicListThrowsException() {
        musicManager = new MusicManager();

        WhoIsTheMostListenedInterpreter query = new WhoIsTheMostListenedInterpreter();
        assertThrows(java.util.NoSuchElementException.class, () -> {
            query.executeQuery(musicManager);
        });
    }
}
