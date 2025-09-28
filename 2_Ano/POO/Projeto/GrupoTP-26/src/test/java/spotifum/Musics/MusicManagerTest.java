package spotifum.Musics;

import java.io.File;
import java.io.IOException;
import java.util.List;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertNotSame;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.junit.jupiter.api.Assertions.assertTrue;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;

import spotifum.Musics.implementations.GeneroMusical;
import spotifum.Musics.implementations.TipoMusica;

/**
 * Unit tests for the MusicManager class, which handles music creation, insertion, 
 * retrieval, deletion, and reproduction. It also tests serialization (save/load) 
 * and cloning functionality.
 *
 * This test class ensures that the MusicManager operates correctly, handling 
 * music entities, including insertion, retrieval, duplication prevention, 
 * reproduction count, and persistence.
 * 
 * @author Afonso Martins (a106931), Luís Felício (a106913), Gonçalo Castro (a107337)
 * @version 12/05/2025
 */
public class MusicManagerTest {

    private MusicManager musicManager;
    private Musica musica1;
    private Musica musica2;

    /**
     * Sets up the test environment.
     */
    @BeforeEach
    public void setUp() {
        musicManager = new MusicManager();
        musica1 = musicManager.createMusica("Song1", "Artist1", "Editora1", "Letra1",
                List.of("audio1"), GeneroMusical.POP, 180, "video1", TipoMusica.NORMAL);
        musica2 = musicManager.createMusica("Song2", "Artist2", "Editora2", "Letra2",
                List.of("audio2"), GeneroMusical.ROCK, 200, "video2", TipoMusica.MULTIMEDIA);
    }

    /**
     * Tests the creation of a music object.
     * Verifies that a music object is successfully created and its properties are correct.
     */
    @Test
    public void testCreateMusicaNormal() {
        assertNotNull(musica1);
        assertEquals("Song1", musica1.getNome());
        assertEquals(180, musica1.getDuracaoSegundos());
    }

    /**
     * Tests the insertion of music into the manager and checks if it exists.
     * Verifies that music is successfully inserted and can be retrieved.
     */
    @Test
    public void testInsertAndExistsMusic() {
        musicManager.insertMusica(musica1);
        assertTrue(musicManager.existsMusic("song1"));
    }

    /**
     * Tests the insertion of a duplicate music.
     * Verifies that inserting a duplicate music throws a runtime exception.
     */
    @Test
    public void testInsertDuplicateThrowsException() {
        musicManager.insertMusica(musica1);
        assertThrows(RuntimeException.class, () -> musicManager.insertMusica(musica1));
    }

    /**
     * Tests the retrieval of music by its name.
     * Verifies that music can be correctly retrieved by its name.
     */
    @Test
    public void testGetMusicaSuccessfully() {
        musicManager.insertMusica(musica2);
        Musica fetched = musicManager.getMusica("song2");
        assertEquals("Song2", fetched.getNome());
    }

    /**
     * Tests the retrieval of music that does not exist.
     * Verifies that attempting to retrieve a non-existent music throws an exception.
     */
    @Test
    public void testGetMusicaNotFoundThrowsException() {
        assertThrows(RuntimeException.class, () -> musicManager.getMusica("inexistente"));
    }

    /**
     * Tests handling music reproduction.
     * Verifies that the reproduction count for a music is correctly incremented after a reproduction.
     */
    @Test
    public void testHandleReproductionIncrementsCount() {
        musicManager.insertMusica(musica1);
        musicManager.handleReproduction(musica1);
        assertEquals(1, musica1.getNumeroReproducoes());
    }

    /**
     * Tests the removal of music from the manager.
     * Verifies that music can be successfully removed and no longer exists in the manager.
     */
    @Test
    public void testRemoveMusicSuccessfully() {
        musicManager.insertMusica(musica2);
        musicManager.removeMusic("song2");
        assertFalse(musicManager.existsMusic("song2"));
    }

    /**
     * Tests the removal of music that does not exist.
     * Verifies that attempting to remove a non-existent music throws an exception.
     */
    @Test
    public void testRemoveMusicNotFoundThrowsException() {
        assertThrows(RuntimeException.class, () -> musicManager.removeMusic("inexistente"));
    }

    /**
     * Tests retrieving all musics from the manager.
     * Verifies that all musics are correctly returned as a list.
     */
    @Test
    public void testGetAllMusicsReturnsCorrectList() {
        musicManager.insertMusica(musica1);
        musicManager.insertMusica(musica2);
        List<Musica> todas = musicManager.getAllMusics();
        assertEquals(2, todas.size());
    }

    /**
     * Tests the cloning of the MusicManager.
     * Verifies that cloning creates a deep copy of the manager with the same musics.
     */
    @Test
    public void testCloneCreatesDeepCopy() {
        musicManager.insertMusica(musica1);
        MusicManager clone = musicManager.clone();
        assertNotSame(clone, musicManager); // Ensure they are different objects
        assertEquals(1, clone.getAllMusics().size()); // Ensure the cloned manager contains the same music
    }

    /**
     * Tests saving and loading of the music manager's data.
     * Verifies that the manager's musics can be saved to a file and later loaded correctly.
     * 
     * @throws IOException if an I/O error occurs while saving/loading the data.
     * @throws ClassNotFoundException if the class of a serialized object cannot be found.
     */
    @Test
    public void testSaveAndLoad() throws IOException, ClassNotFoundException {
        String path = "test_music_data.dat";

        // Prepare and save music data
        musicManager.insertMusica(musica1);
        musicManager.insertMusica(musica2);
        musicManager.save(path);

        // Load data into a new manager instance
        MusicManager loadedManager = new MusicManager();
        loadedManager.load(path);

        assertEquals(2, loadedManager.getAllMusics().size()); // Verify the correct number of musics
        assertTrue(loadedManager.existsMusic("song1")); // Verify music1 exists
        assertTrue(loadedManager.existsMusic("song2")); // Verify music2 exists

        // Clean up the test file
        new File(path).delete();
    }
}
