package spotifum.queries;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertNull;
import static org.junit.jupiter.api.Assertions.assertTrue;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;

import spotifum.Users.PlanoSubscricao;
import spotifum.Users.UserManager;
import spotifum.Users.Utilizador;

/**
 * The tests for the WhoIsTheUserWithTheMostPoints class.
 *
 * This class contains tests for the constructors, methods, and operations of the WhoIsTheUserWithTheMostPoints class,
 *
 * @author  Afonso Martins (a106931), Luís Felício (a106913) and Goncalo Castro (a107337)
 * @version (17052025)
 */
public class WhoIsTheUserWithTheMostPointsTest {

    private UserManager userManager;

    /**
     * Sets up the test environment.
     */
    @BeforeEach
    void setUp() {
        userManager = new UserManager();

        Utilizador u1 = userManager.createUser("User 1", "user1@example.com", "123", PlanoSubscricao.FREE);
        Utilizador u2 = userManager.createUser("User 2", "user2@example.com", "123", PlanoSubscricao.FREE);
        Utilizador u3 = userManager.createUser("User 3", "user3@example.com", "123", PlanoSubscricao.FREE);

        u1.setPontos(10.5);
        u2.setPontos(25.0); // mais pontos
        u3.setPontos(5.0);

        userManager.insertUser(u1);
        userManager.insertUser(u2);
        userManager.insertUser(u3);
    }

    /**
    * Test the constructor of the WhoIsTheUserWithTheMostPoints class.
    */
    @Test
    void testUserWithMostPoints() {
        WhoIsTheUserWithTheMostPoints query = new WhoIsTheUserWithTheMostPoints();
        Utilizador result = query.executeQuery(userManager);
        assertNotNull(result);
        assertEquals("user2@example.com", result.getEmail());
    }

    /**
    * Test the constructor of the WhoIsTheUserWithTheMostPoints class.
    */
    @Test
    void testNoUsersReturnsNull() {
        userManager = new UserManager(); // vazio
        WhoIsTheUserWithTheMostPoints query = new WhoIsTheUserWithTheMostPoints();
        Utilizador result = query.executeQuery(userManager);
        assertNull(result);
    }

    /**
    * Test the constructor of the WhoIsTheUserWithTheMostPoints class.
    */
    @Test
    void testTieReturnsFirstEncountered() {
        userManager = new UserManager();

        Utilizador u1 = userManager.createUser("User A", "a@example.com", "123", PlanoSubscricao.FREE);
        Utilizador u2 = userManager.createUser("User B", "b@example.com", "123", PlanoSubscricao.FREE);

        u1.setPontos(30.0);
        u2.setPontos(30.0); // empate

        userManager.insertUser(u1);
        userManager.insertUser(u2);

        WhoIsTheUserWithTheMostPoints query = new WhoIsTheUserWithTheMostPoints();
        Utilizador result = query.executeQuery(userManager);

        assertTrue(result.getEmail().equals("a@example.com") || result.getEmail().equals("b@example.com"));
    }
}
