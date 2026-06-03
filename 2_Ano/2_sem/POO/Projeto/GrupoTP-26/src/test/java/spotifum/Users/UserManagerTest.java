package spotifum.Users;

import java.time.LocalDateTime;
import java.util.List;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.junit.jupiter.api.Assertions.assertTrue;
import org.junit.jupiter.api.Test;

import spotifum.Exceptions.UserAlreadyExistsException;
import spotifum.Exceptions.UserNotFoundException;
import spotifum.Musics.MusicManager;
import spotifum.Musics.Musica;
import spotifum.Musics.implementations.GeneroMusical;
import spotifum.Musics.implementations.TipoMusica;

/**
 * The tests for the UserManager class.
 *
 * This class contains unit tests for the creation, insertion, retrieval,
 * and deletion of users, as well as interactions with favorite music and reproduction logging.
 *
 * @author  Afonso Martins (a106931), Luís Felício (a106913) and Goncalo Castro (a107337)
 * @version (17052025)
 */
public class UserManagerTest {

    /**
     * Tests the constructor of UserManager.
     */
    @Test
    public void testConstructor() {
        UserManager userManager = new UserManager();
        assertNotNull(userManager);
    }

    /**
     * Tests creating a user with valid input fields.
     */
    @Test
    public void testCreateUserValidInputs() {
        UserManager userManager = new UserManager();
        Utilizador user = userManager.createUser("João", "joao@mail.com", "Rua A", PlanoSubscricao.PremiumBase);
        assertNotNull(user);
        assertEquals("João", user.getNome());
        assertEquals("joao@mail.com", user.getEmail());
        assertEquals("Rua A", user.getMorada());
        assertEquals(PlanoSubscricao.PremiumBase, user.getPlano());
    }

    /**
     * Tests creating a user with invalid inputs (null values).
     */
    @Test
    public void testCreateUserInvalidInputs() {
        UserManager userManager = new UserManager();
        assertThrows(IllegalArgumentException.class, () -> {
            userManager.createUser(null, "joao@mail.com", "Rua A", PlanoSubscricao.PremiumBase);
        });
        assertThrows(IllegalArgumentException.class, () -> {
            userManager.createUser("João", null, "Rua A", PlanoSubscricao.PremiumBase);
        });
        assertThrows(IllegalArgumentException.class, () -> {
            userManager.createUser("João", "joao@mail.com", null, PlanoSubscricao.PremiumBase);
        });
        assertThrows(IllegalArgumentException.class, () -> {
            userManager.createUser("João", "joao@mail.com", "Rua A", null);
        });
    }

    /**
     * Tests inserting and retrieving a user.
     */
    @Test
    public void testCreateAndInsertUser() {
        UserManager userManager = new UserManager();
        Utilizador user = userManager.createUser("João", "joao@mail.com", "Rua A", PlanoSubscricao.PremiumBase);
        userManager.insertUser(user);

        Utilizador retrieved = userManager.getUser("joao@mail.com");
        assertEquals("João", retrieved.getNome());
    }

    /**
     * Tests handling of inserting a duplicate user.
     */
    @Test
    public void testCreateUserDuplicate() {
        UserManager userManager = new UserManager();
        Utilizador user = userManager.createUser("Ana", "ana@mail.com", "Rua B", PlanoSubscricao.PremiumBase);
        userManager.insertUser(user);

        assertThrows(UserAlreadyExistsException.class, () -> {
            userManager.insertUser(user);
        });
    }

    /**
     * Tests removing a user and ensuring they no longer exist.
     */
    @Test
    public void testRemoveUser() {
        UserManager userManager = new UserManager();
        Utilizador user = userManager.createUser("Carlos", "carlos@mail.com", "Rua C", PlanoSubscricao.PremiumBase);
        userManager.insertUser(user);

        userManager.removeUser("carlos@mail.com");
        assertThrows(UserNotFoundException.class, () -> userManager.getUser("carlos@mail.com"));
    }

    /**
     * Tests retrieving a user by their email.
     */
    @Test
    public void testGetUserbyEmail() {
        UserManager userManager = new UserManager();
        Utilizador user = userManager.createUser("Carlos", "carlos@mail.com", "Rua C", PlanoSubscricao.PremiumBase);
        userManager.insertUser(user);

        Utilizador retrieved = userManager.getUser("carlos@mail.com");
        assertEquals("Carlos", retrieved.getNome());
        assertEquals("Rua C", retrieved.getMorada());
        assertEquals(PlanoSubscricao.PremiumBase, retrieved.getPlano());
    }

    /**
     * Tests adding a favorite music to a user's list.
     */
    @Test
    public void testAddFavoriteMusic() {
        UserManager userManager = new UserManager();
        MusicManager musicManager = new MusicManager();
        Musica musica = musicManager.createMusica(
            "Let It Be",
            "The Beatles",
            "Editor1",
            "Lyrics1",
            List.of("Track1"),
            GeneroMusical.ROCK,
            180,
            "Let It Be Album",
            TipoMusica.NORMAL
        );
        musicManager.insertMusica(musica);

        Utilizador user = userManager.createUser("Maria", "maria@mail.com", "Rua D", PlanoSubscricao.PremiumBase);
        userManager.insertUser(user);

        userManager.addFavoriteMusic(musicManager, "Let It Be", "maria@mail.com");

        assertTrue(user.getMusicasFavoritas().contains(musica));
    }

    /**
     * Tests logging a music reproduction for a user.
     */
    @Test
    public void testAddMusicReproduction() {
        UserManager userManager = new UserManager();
        MusicManager musicManager = new MusicManager();
        Musica musica = musicManager.createMusica(
            "Yesterday",
            "The Beatles",
            "Editor2",
            "Lyrics2",
            List.of("Track2"),
            GeneroMusical.ROCK,
            200,
            "Yesterday Album",
            TipoMusica.NORMAL
        );
        musicManager.insertMusica(musica);

        Utilizador user = userManager.createUser("Pedro", "pedro@mail.com", "Rua E", PlanoSubscricao.PremiumBase);
        userManager.insertUser(user);

        userManager.addMusicReproduction(musicManager, "Yesterday", "pedro@mail.com", LocalDateTime.now());

        assertEquals(1, musica.getNumeroReproducoes());
    }

    /**
     * Tests retrieving all users.
     */
    @Test
    public void testGetAllUsers() {
        UserManager userManager = new UserManager();
        userManager.insertUser(userManager.createUser("A", "a@mail.com", "Rua X", PlanoSubscricao.PremiumBase));
        userManager.insertUser(userManager.createUser("B", "b@mail.com", "Rua Y", PlanoSubscricao.PremiumBase));

        List<Utilizador> users = userManager.getAllUsers();
        assertEquals(2, users.size());
    }
}
