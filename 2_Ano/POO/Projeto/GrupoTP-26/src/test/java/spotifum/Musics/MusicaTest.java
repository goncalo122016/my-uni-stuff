package spotifum.Musics;

import java.util.List;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertNotSame;
import static org.junit.jupiter.api.Assertions.assertTrue;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;

import spotifum.Musics.implementations.GeneroMusical;

/**
 * Unit tests for the Musica class, which represents a music entity with attributes
 * such as name, artist, publisher, lyrics, musical genre, duration, and play count.
 * The tests verify the behavior of the constructor, getters, setters, copy constructor,
 * equality comparison, cloning, and other methods of the Musica class.
 * 
 * @author Afonso Martins (a106931),Luís Felício (a106913),Gonçalo Castro (a107337)
 * @version (17052025)
 */
public class MusicaTest {

    private Musica musica1;
    private Musica musica2;

    /**
     * Sets up the test environment.
     */
    @BeforeEach
    public void setUp() {
        musica1 = new Musica("Nome", "Interprete", "Editora", "Letra",
                List.of("do", "re", "mi"), GeneroMusical.ROCK, 240);
        musica2 = new Musica(musica1); // Copy constructor
    }

    /**
     * Tests the constructor and getter methods of the Musica class.
     * Verifies that all attributes are set correctly upon object creation.
     */
    @Test
    public void testConstructorAndGetters() {
        assertEquals("Nome", musica1.getNome());
        assertEquals("Interprete", musica1.getInterprete());
        assertEquals("Editora", musica1.getNomeEditora());
        assertEquals("Letra", musica1.getLetra());
        assertEquals(List.of("do", "re", "mi"), musica1.getMusica());
        assertEquals(GeneroMusical.ROCK, musica1.getGeneroMusical());
        assertEquals(240, musica1.getDuracaoSegundos());
        assertEquals(0, musica1.getNumeroReproducoes());
    }

    /**
     * Tests the copy constructor of the Musica class.
     * Verifies that a new object is created as a copy of an existing object,
     * ensuring they are not the same instance but have the same data.
     */
    @Test
    public void testCopyConstructor() {
        assertEquals(musica1, musica2);
        assertNotSame(musica1, musica2); // Ensure the objects are not the same instance
    }

    /**
     * Tests the reproduzir() method, which increments the play count.
     * Verifies that the number of reproductions is incremented correctly after calling reproduzir().
     */
    @Test
    public void testReproduzir() {
        musica1.reproduzir();
        assertEquals(1, musica1.getNumeroReproducoes());
    }

    /**
     * Tests the setter methods of the Musica class.
     * Verifies that the attributes of the Musica object can be modified correctly.
     */
    @Test
    public void testSetters() {
        musica1.setNome("Nova");
        musica1.setInterprete("Outro");
        musica1.setNomeEditora("NovaEditora");
        musica1.setLetra("Nova Letra");
        musica1.setMusica(List.of("fa", "sol"));
        musica1.setGeneroMusical("JAZZ");
        musica1.setDuracaoSegundos(180);
        musica1.setNumeroReproducoes(10);

        assertEquals("Nova", musica1.getNome());
        assertEquals("Outro", musica1.getInterprete());
        assertEquals("NovaEditora", musica1.getNomeEditora());
        assertEquals("Nova Letra", musica1.getLetra());
        assertEquals(List.of("fa", "sol"), musica1.getMusica());
        assertEquals(GeneroMusical.JAZZ, musica1.getGeneroMusical());
        assertEquals(180, musica1.getDuracaoSegundos());
        assertEquals(10, musica1.getNumeroReproducoes());
    }

    /**
     * Tests the behavior when an invalid genre is set using the setGeneroMusical() method.
     * Verifies that an invalid genre value is defaulted to 'OUTRO'.
     */
    @Test
    public void testSetGeneroMusicalComValorInvalido() {
        musica1.setGeneroMusical("inexistente");
        assertEquals(GeneroMusical.OUTRO, musica1.getGeneroMusical());
    }

    /**
     * Tests the cloning functionality of the Musica class.
     * Verifies that the clone() method creates a deep copy of the Musica object.
     */
    @Test
    public void testCloneCreatesDeepCopy() {
        Musica clone = musica1.clone();
        assertEquals(musica1, clone);
        assertNotSame(musica1, clone); // Ensure the objects are not the same instance
    }

    /**
     * Tests the equals() method for Musica objects.
     * Verifies that two Musica objects are considered equal if their attributes are the same.
     * Also tests inequality when a single attribute is different.
     */
    @Test
    public void testEqualsTrueAndFalse() {
        assertTrue(musica1.equals(musica2));
        musica2.setNome("Diferente");
        assertFalse(musica1.equals(musica2));
    }

    /**
     * Tests the toString() method of the Musica class.
     * Verifies that the string representation of the object contains key attributes such as name,
     * artist, lyrics, and genre.
     */
    @Test
    public void testToStringIncludesData() {
        String toStr = musica1.toString();
        assertTrue(toStr.contains("Nome"));
        assertTrue(toStr.contains("Interprete"));
        assertTrue(toStr.contains("Letra"));
        assertTrue(toStr.contains("ROCK"));
    }

    /**
     * Tests the default constructor of the Musica class.
     * Verifies that the default constructor initializes the object with default values.
     */
    @Test
    public void testDefaultConstructor() {
        Musica musica = new Musica();
        assertEquals("", musica.getNome());
        assertEquals(GeneroMusical.OUTRO, musica.getGeneroMusical());
        assertNotNull(musica.getMusica());
        assertEquals(0, musica.getNumeroReproducoes());
    }
}
