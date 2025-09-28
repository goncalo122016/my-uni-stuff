package spotifum.queries;

import java.time.LocalDateTime;
import java.util.List;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNull;
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
 * The tests for the UserThatListenedToMoreMusics class.
 *
 * This class contains tests for the constructors, methods, and operations of the UserThatListenedToMoreMusics class,
 *
 * @author  Afonso Martins (a106931), Luís Felício (a106913) and Goncalo Castro (a107337)
 * @version (17052025)
 */
public class UserThatListenedToMoreMusicsTest {

    private UserManager userManager;
    private MusicManager musicManager;
    private Musica musica;

    /**
     * Sets up the test environment.
     */
    @BeforeEach
    void setUp() {
        userManager = new UserManager();
        musicManager = new MusicManager();

        Utilizador u1 = userManager.createUser("User 1", "u1@example.com", "pass", PlanoSubscricao.FREE);
        Utilizador u2 = userManager.createUser("User 2", "u2@example.com", "pass", PlanoSubscricao.FREE);

        userManager.insertUser(u1);
        userManager.insertUser(u2);

        musica = musicManager.createMusica("Song", "Artist", "Editor", "Lyrics", List.of("Track"), GeneroMusical.POP, 200, "Album", TipoMusica.NORMAL);
        musicManager.insertMusica(musica);

        userManager.addMusicReproduction(musicManager,"Song","u1@example.com", LocalDateTime.of(2024, 5, 1, 10, 0));
        userManager.addMusicReproduction(musicManager,"Song","u1@example.com",  LocalDateTime.of(2024, 5, 2, 12, 0));

        userManager.addMusicReproduction(musicManager, "Song", "u2@example.com", LocalDateTime.of(2024, 5, 3, 14, 0));
    }

    /**
    * Test the constructor of the UserThatListenedToMoreMusics class.
    */
    @Test
    void testExecuteQueryWithoutDateRange() {
        UserThatListenedToMoreMusics query = new UserThatListenedToMoreMusics();
        Utilizador result = query.executeQuery(userManager);
        assertEquals("u1@example.com", result.getEmail());
    }

    /**
    * Test the constructor of the UserThatListenedToMoreMusics class.
    */
    @Test
    void testExecuteQueryWithDateRange() {
        UserThatListenedToMoreMusics query = new UserThatListenedToMoreMusics();
        LocalDateTime start = LocalDateTime.of(2024, 5, 1, 0, 0);
        LocalDateTime end = LocalDateTime.of(2024, 5, 2, 23, 59);

        Utilizador result = query.executeQuery(userManager, start, end);
        assertEquals("u1@example.com", result.getEmail());
    }

    /**
    * Test the constructor of the UserThatListenedToMoreMusics class.
    */
    @Test
    void testExecuteQueryWithNoReproductionsInDateRange() {
        UserThatListenedToMoreMusics query = new UserThatListenedToMoreMusics();
        LocalDateTime start = LocalDateTime.of(2020, 1, 1, 0, 0);
        LocalDateTime end = LocalDateTime.of(2020, 12, 31, 23, 59);

        Utilizador result = query.executeQuery(userManager, start, end);
        assertNull(result);
    }

    /**
    * Test the constructor of the UserThatListenedToMoreMusics class.
    */
    @Test
    void testExecuteQueryWithInvalidDates() {
        UserThatListenedToMoreMusics query = new UserThatListenedToMoreMusics();
        LocalDateTime start = LocalDateTime.of(2024, 6, 1, 0, 0);
        LocalDateTime end = LocalDateTime.of(2024, 5, 1, 0, 0);

        assertThrows(RuntimeException.class, () -> query.executeQuery(userManager, start, end));
    }
}
