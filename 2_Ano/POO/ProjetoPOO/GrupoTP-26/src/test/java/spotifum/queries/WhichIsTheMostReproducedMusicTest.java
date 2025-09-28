package spotifum.queries;

import java.util.List;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertNull;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;

import spotifum.Musics.MusicManager;
import spotifum.Musics.Musica;
import spotifum.Musics.implementations.GeneroMusical;
import spotifum.Musics.implementations.TipoMusica;

/**
 * The tests for the WhichIsTheMostReproducedMusic class.
 *
 * This class contains tests for the constructors, methods, and operations of the WhichIsTheMostReproducedMusic class,
 *
 * @author  Afonso Martins (a106931), Luís Felício (a106913) and Goncalo Castro (a107337)
 * @version (17052025)
 */
public class WhichIsTheMostReproducedMusicTest {

    private MusicManager musicManager;

    /**
     * Sets up the test environment.
    */
    @BeforeEach
    void setUp() {
        musicManager = new MusicManager();

        Musica m1 = musicManager.createMusica("Top Hit", "Artist1", "Editor1", "Lyrics1", List.of("Track1"), GeneroMusical.POP, 200, "Album1", TipoMusica.NORMAL);
        Musica m2 = musicManager.createMusica("Runner Up", "Artist2", "Editor2", "Lyrics2", List.of("Track2"), GeneroMusical.ROCK, 180, "Album2", TipoMusica.NORMAL);
        Musica m3 = musicManager.createMusica("Least Played", "Artist3", "Editor3", "Lyrics3", List.of("Track3"), GeneroMusical.JAZZ, 220, "Album3", TipoMusica.NORMAL);

        // Reproduções: m1 = 3, m2 = 2, m3 = 0
        m1.reproduzir();
        m1.reproduzir();
        m1.reproduzir();

        m2.reproduzir();
        m2.reproduzir();

        musicManager.insertMusica(m1);
        musicManager.insertMusica(m2);
        musicManager.insertMusica(m3);
    }

    /**
    * Test the constructor of the WhichIsTheMostReproducedMusic class.
    */
    @Test
    void testMostReproducedMusicIsCorrect() {
        WhichIsTheMostReproducedMusic query = new WhichIsTheMostReproducedMusic();
        Musica result = query.executeQuery(musicManager);
        assertNotNull(result);
        assertEquals("Top Hit", result.getNome());
    }

    /**
    * Test the constructor of the WhichIsTheMostReproducedMusic class.
    */
    @Test
    void testEmptyMusicListReturnsNull() {
        musicManager = new MusicManager(); // reinicia para vazio
        WhichIsTheMostReproducedMusic query = new WhichIsTheMostReproducedMusic();
        Musica result = query.executeQuery(musicManager);
        assertNull(result);
    }

    /**
    * Test the constructor of the WhichIsTheMostReproducedMusic class.
    */
    @Test
    void testTieReturnsFirstInserted() {
        musicManager = new MusicManager();

        Musica m1 = musicManager.createMusica("Tie A", "Artist1", "Editor1", "Lyrics1", List.of("Track1"), GeneroMusical.POP, 200, "Album1", TipoMusica.NORMAL);
        Musica m2 = musicManager.createMusica("Tie B", "Artist2", "Editor2", "Lyrics2", List.of("Track2"), GeneroMusical.ROCK, 180, "Album2", TipoMusica.NORMAL);

        // Ambos com 2 reproduções
        m1.reproduzir();
        m1.reproduzir();

        m2.reproduzir();
        m2.reproduzir();

        musicManager.insertMusica(m1);
        musicManager.insertMusica(m2);

        WhichIsTheMostReproducedMusic query = new WhichIsTheMostReproducedMusic();
        Musica result = query.executeQuery(musicManager);

        assertNotNull(result);
        assertEquals("Tie B", result.getNome()); 
    }
}
