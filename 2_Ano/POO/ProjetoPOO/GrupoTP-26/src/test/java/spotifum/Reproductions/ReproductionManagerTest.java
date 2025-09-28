package spotifum.Reproductions;

import java.io.File;
import java.time.LocalDateTime;
import java.util.List;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.junit.jupiter.api.Assertions.assertTrue;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;

/**
 * The tests for the ReproductionManager class.
 *
 * This class contains tests for the constructors, methods, and operations of the ReproductionManager class.
 *
 * @author  Afonso Martins (a106931), Luís Felício (a106913) and Goncalo Castro (a107337)
 * @version (17052025)
 */
public class ReproductionManagerTest {

    private ReproductionManager manager;
    private ReproducaoMusical sampleReproducao;

    /**
     * Initializes a clean test environment with one sample reproduction.
     */
    @BeforeEach
    void setUp() {
        manager = new ReproductionManager();
        sampleReproducao = new ReproducaoMusical("Song A", LocalDateTime.now(), "user@example.com");
        manager.addReproducao(sampleReproducao);
    }

    /**
     * Tests the creation of a new music reproduction.
     */
    @Test
    void testCreateReproducao() {
        manager.createReproducao("Song B", "another@example.com", LocalDateTime.now());
        List<ReproducaoMusical> all = manager.getAllReproducoes();
        assertEquals(2, all.size(), "Should contain two reproductions after creation.");
    }

    /**
     * Tests the creation of a reproduction with null arguments.
     */
    @Test
    void testCreateReproducaoWithNullArguments() {
        assertThrows(IllegalArgumentException.class,
                () -> manager.createReproducao(null, "email", LocalDateTime.now()),
                "Null music name should throw exception.");
        assertThrows(IllegalArgumentException.class,
                () -> manager.createReproducao("song", null, LocalDateTime.now()),
                "Null email should throw exception.");
        assertThrows(IllegalArgumentException.class,
                () -> manager.createReproducao("song", "email", null),
                "Null date should throw exception.");
    }

    /**
     * Tests adding a null reproduction.
     */
    @Test
    void testAddReproducaoWithNull() {
        assertThrows(IllegalArgumentException.class,
                () -> manager.addReproducao(null),
                "Adding null reproduction should throw exception.");
    }

    /**
     * Tests removing an existing reproduction.
     */
    @Test
    void testRemoveReproducao() {
        manager.removeReproducao("Song A", "user@example.com");
        assertTrue(manager.getAllReproducoes().isEmpty(),
                "Reproductions list should be empty after removal.");
    }

    /**
     * Tests removing with invalid arguments.
     */
    @Test
    void testRemoveReproducaoWithInvalidArguments() {
        assertThrows(IllegalArgumentException.class,
                () -> manager.removeReproducao(null, "email"),
                "Null music name should throw exception.");
        assertThrows(IllegalArgumentException.class,
                () -> manager.removeReproducao("song", null),
                "Null email should throw exception.");
    }

    /**
     * Tests retrieving all reproductions.
     */
    @Test
    void testGetAllReproducoes() {
        List<ReproducaoMusical> all = manager.getAllReproducoes();
        assertEquals(1, all.size(), "Should contain one reproduction.");
        assertEquals("Song A", all.get(0).getMusica(), "Music name should be 'Song A'.");
    }

    /**
     * Tests filtering reproductions by email.
     */
    @Test
    void testGetReproducoesByEmail() {
        List<ReproducaoMusical> result = manager.getReproducoesByEmail("user@example.com");
        assertEquals(1, result.size(), "Should return one reproduction for the given email.");
    }

    /**
     * Tests filtering reproductions with null email.
     */
    @Test
    void testGetReproducoesByEmailInvalid() {
        assertThrows(IllegalArgumentException.class,
                () -> manager.getReproducoesByEmail(null),
                "Null email should throw exception.");
    }

    /**
     * Tests filtering reproductions by music name.
     */
    @Test
    void testGetReproducoesByMusica() {
        List<ReproducaoMusical> result = manager.getReproducoesByMusica("Song A");
        assertEquals(1, result.size(), "Should return one reproduction for the given music.");
    }

    /**
     * Tests filtering reproductions with null music name.
     */
    @Test
    void testGetReproducoesByMusicaInvalid() {
        assertThrows(IllegalArgumentException.class,
                () -> manager.getReproducoesByMusica(null),
                "Null music name should throw exception.");
    }

    /**
     * Tests saving and loading data from a file.
     */
    @Test
    void testSaveAndLoad() throws Exception {
        String path = "test_reproducoes.ser";
        manager.save(path);

        ReproductionManager newManager = new ReproductionManager();
        newManager.load(path);

        List<ReproducaoMusical> loaded = newManager.getAllReproducoes();
        assertEquals(1, loaded.size(), "After loading, should contain one reproduction.");
        assertEquals("Song A", loaded.get(0).getMusica(), "Loaded music name should be 'Song A'.");

        // Cleanup
        new File(path).delete();
    }
}
