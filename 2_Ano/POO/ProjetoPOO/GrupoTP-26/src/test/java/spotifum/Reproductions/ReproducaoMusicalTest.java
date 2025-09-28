package spotifum.Reproductions;

import java.time.LocalDateTime;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNotEquals;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertNotSame;
import static org.junit.jupiter.api.Assertions.assertTrue;
import org.junit.jupiter.api.Test;

/**
 * Testes para a classe ReproducaoMusical.
 *
 * Esta classe testa os construtores, métodos de acesso, clone,
 * igualdade e representação textual da classe ReproducaoMusical.
 *
 * @author Afonso Martins (a106931),Luís Felício (a106913),Gonçalo Castro (a107337)
 * @version (17052025)
 */
public class ReproducaoMusicalTest {

    /**
     * Testa os construtores da classe ReproducaoMusical.
     */
    @Test
    public void testConstructors() {
        ReproducaoMusical r1 = new ReproducaoMusical();
        assertNotNull(r1, "Instância por defeito deve ser criada com sucesso.");

        LocalDateTime now = LocalDateTime.now();
        ReproducaoMusical r2 = new ReproducaoMusical("musica.mp3", now, "user@email.com");
        assertNotNull(r2, "Construtor parametrizado deve criar uma instância não nula.");

        ReproducaoMusical r3 = new ReproducaoMusical(r2);
        assertNotNull(r3, "Construtor de cópia deve criar uma instância não nula.");
        assertEquals(r2, r3, "Construtor de cópia deve criar uma instância igual à original.");
    }

    /**
     * Testa os getters e setters da classe ReproducaoMusical.
     */
    @Test
    public void testGettersAndSetters() {
        ReproducaoMusical r = new ReproducaoMusical();
        LocalDateTime dt = LocalDateTime.of(2023, 5, 10, 14, 30);

        r.setMusica("exemplo.mp3");
        r.setDataReproducao(dt);
        r.setEmail("teste@spotifum.com");

        assertEquals("exemplo.mp3", r.getMusica(), "Getter musica deve retornar o valor correto.");
        assertEquals(dt, r.getDataReproducao(), "Getter dataReproducao deve retornar o valor correto.");
        assertEquals("teste@spotifum.com", r.getEmail(), "Getter email deve retornar o valor correto.");
    }

    /**
     * Testa o método clone da classe ReproducaoMusical.
     */
    @Test
    public void testClone() {
        ReproducaoMusical r1 = new ReproducaoMusical("music.mp3", LocalDateTime.now(), "a@a.com");
        ReproducaoMusical r2 = r1.clone();

        assertEquals(r1, r2, "Clone deve ser igual ao objeto original.");
        assertNotSame(r1, r2, "Clone deve ser uma instância diferente.");
    }

    /**
     * Testa o método equals da classe ReproducaoMusical.
     */
    @Test
    public void testEquals() {
        LocalDateTime data = LocalDateTime.of(2025, 5, 11, 12, 0);
        ReproducaoMusical r1 = new ReproducaoMusical("x.mp3", data, "a@a.com");
        ReproducaoMusical r2 = new ReproducaoMusical("x.mp3", data, "a@a.com");
        ReproducaoMusical r3 = new ReproducaoMusical("y.mp3", data, "a@a.com");

        assertEquals(r1, r2, "Objetos com os mesmos dados devem ser iguais.");
        assertNotEquals(r1, r3, "Objetos com dados diferentes não devem ser iguais.");
        assertNotEquals(r1, null, "Deve retornar falso quando comparado com null.");
        assertNotEquals(r1, "string", "Deve retornar falso quando comparado com outro tipo.");
    }

    /**
     * Testa o método toString da classe ReproducaoMusical.
     */
    @Test
    public void testToString() {
        LocalDateTime data = LocalDateTime.of(2025, 5, 11, 10, 0);
        ReproducaoMusical r = new ReproducaoMusical("x.mp3", data, "a@a.com");

        String str = r.toString();
        assertTrue(str.contains("x.mp3"), "toString deve conter o nome da música.");
        assertTrue(str.contains("a@a.com"), "toString deve conter o email.");
        assertTrue(str.contains("2025"), "toString deve conter a data.");
    }
}
