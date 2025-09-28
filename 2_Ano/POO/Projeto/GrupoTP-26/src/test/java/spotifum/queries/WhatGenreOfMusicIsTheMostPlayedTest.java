package spotifum.queries;

import java.util.List;

import static org.junit.jupiter.api.Assertions.assertEquals;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;

import spotifum.Musics.MusicManager;
import spotifum.Musics.Musica;
import spotifum.Musics.implementations.GeneroMusical;
import spotifum.Musics.implementations.TipoMusica;

/**
 * The tests for the WhatGenreOfMusicIsTheMostPlayed class.
 *
 * This class contains tests for the constructors, methods, and operations of the WhatGenreOfMusicIsTheMostPlayed class,
 *
 * @author  Afonso Martins (a106931), Luís Felício (a106913) and Goncalo Castro (a107337)
 * @version (17052025)
 */
public class WhatGenreOfMusicIsTheMostPlayedTest {

    private MusicManager musicManager;

    /**
     * Sets up the test environment.
     */
    @BeforeEach
    void setUp() {
        musicManager = new MusicManager();

        Musica m1 = musicManager.createMusica("Pop Hit", "Artist1", "Editor1", "Lyrics1", List.of("Track1"), GeneroMusical.POP, 200, "Pop Album", TipoMusica.NORMAL);
        Musica m2 = musicManager.createMusica("Rock Song", "Artist2", "Editor2", "Lyrics2", List.of("Track2"), GeneroMusical.ROCK, 220, "Rock Album", TipoMusica.NORMAL);
        Musica m3 = musicManager.createMusica("Jazz Flow", "Artist3", "Editor3", "Lyrics3", List.of("Track3"), GeneroMusical.JAZZ, 180, "Jazz Album", TipoMusica.NORMAL);

        m1.reproduzir(); // POP: 1
        m1.reproduzir(); // POP: 2
        m2.reproduzir(); // ROCK: 1
        m3.reproduzir(); // JAZZ: 1

        musicManager.insertMusica(m1);
        musicManager.insertMusica(m2);
        musicManager.insertMusica(m3);
    }

    /**
    * Test the constructor of the WhatGenreOfMusicIsTheMostPlayed class.
    */
    @Test
    void testMostPlayedGenreIsPop() {
        WhatGenreOfMusicIsTheMostPlayed query = new WhatGenreOfMusicIsTheMostPlayed();
        GeneroMusical result = query.executeQuery(musicManager);
        assertEquals(GeneroMusical.POP, result);
    }

    /**
    * Test the constructor of the WhatGenreOfMusicIsTheMostPlayed class.
    */
    @Test
    void testMostPlayedGenreIsRock() {
        musicManager = new MusicManager();

        Musica m1 = musicManager.createMusica("Rock Anthem", "ArtistX", "EditorX", "LyricsX", List.of("TrackX"), GeneroMusical.ROCK, 200, "AlbumX", TipoMusica.NORMAL);
        m1.reproduzir();
        m1.reproduzir();
        m1.reproduzir(); // ROCK: 3

        musicManager.insertMusica(m1);

        WhatGenreOfMusicIsTheMostPlayed query = new WhatGenreOfMusicIsTheMostPlayed();
        GeneroMusical result = query.executeQuery(musicManager);
        assertEquals(GeneroMusical.ROCK, result);
    }

    /**
    * Test the constructor of the WhatGenreOfMusicIsTheMostPlayed class.
    */
    @Test
    void testWithNoMusicsReturnsDefaultGenre() {
        musicManager = new MusicManager();

        WhatGenreOfMusicIsTheMostPlayed query = new WhatGenreOfMusicIsTheMostPlayed();
        GeneroMusical result = query.executeQuery(musicManager);
        assertEquals(GeneroMusical.POP, result); 
    }

    /**
    * Test the constructor of the WhatGenreOfMusicIsTheMostPlayed class.
    */
    @Test
    void testWithTieReturnsFirstGenreWithMax() {
        musicManager = new MusicManager();

        Musica m1 = musicManager.createMusica("Pop1", "A", "E", "L", List.of("T"), GeneroMusical.POP, 100, "A", TipoMusica.NORMAL);
        Musica m2 = musicManager.createMusica("Rock1", "A", "E", "L", List.of("T"), GeneroMusical.ROCK, 100, "A", TipoMusica.NORMAL);

        m1.reproduzir(); // POP: 1
        m2.reproduzir(); // ROCK: 1

        musicManager.insertMusica(m1);
        musicManager.insertMusica(m2);

        WhatGenreOfMusicIsTheMostPlayed query = new WhatGenreOfMusicIsTheMostPlayed();
        GeneroMusical result = query.executeQuery(musicManager);
        assertEquals(GeneroMusical.POP, result);
    }
}
