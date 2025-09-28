package spotifum.Playlists;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.junit.jupiter.api.Assertions.assertTrue;
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
 * Unit tests for the FavoriteMusics class and related playlist behavior.
 *
 * This test class ensures that favorite music playlists are correctly created, managed,
 * and validated according to user subscription plans. It verifies correct instantiation,
 * music insertion/removal, and error handling for free users.
 * 
 * @author Afonso Martins (a106931),Luís Felício (a106913),Gonçalo Castro (a107337)
 * @version (17052025)
 */
public class FavoriteMusicsTest {

    private Utilizador userPremiumBase;
    private Utilizador userPremiumTop;
    private Utilizador userFree;
    private List<Musica> musicasList;
    private UserManager userManager;
    private MusicManager musicManager;
    private PlaylistManager playlistManager;

    /**
     * Sets up the test environment.
     */
    @BeforeEach
    public void setUp() {
        this.userManager = new UserManager();
        musicManager = new MusicManager();
        playlistManager = new PlaylistManager(); 

        userPremiumBase = userManager.createUser("User Premium Base", "premium.base@example.com", "pw123", PlanoSubscricao.PremiumBase);
        userPremiumTop = userManager.createUser("User Premium Top", "premium.top@example.com", "pw123", PlanoSubscricao.PremiumTop);
        userFree = userManager.createUser("User Free", "free@example.com", "pw123", PlanoSubscricao.FREE);
        userManager.insertUser(userPremiumBase);
        userManager.insertUser(userPremiumTop);
        userManager.insertUser(userFree);
        
        Musica m1 = musicManager.createMusica("Song1", "Artist1", "Editor1", "Lyrics1", List.of("Track1"), GeneroMusical.POP, 180, "Album1", TipoMusica.NORMAL);
        Musica m2 = musicManager.createMusica("Song2", "Artist2", "Editor2", "Lyrics2", List.of("Track2"), GeneroMusical.ROCK, 200, "Album2", TipoMusica.NORMAL);
        Musica m3 = musicManager.createMusica("Song3", "Artist3", "Editor3", "Lyrics3", List.of("Track3"), GeneroMusical.HIPHOP, 240, "Album3", TipoMusica.NORMAL);
        musicManager.insertMusica(m1);
        musicManager.insertMusica(m2);
        musicManager.insertMusica(m3);

        musicasList = Arrays.asList(m1, m2, m3);
    }

    /**
     * Tests creation of a favorite playlist for a Premium Base user.
     */
    @Test
    public void testCreateFavoriteMusicsForPremiumBaseUser() {
        playlistManager.createFavoriteMusicsPlaylist(userPremiumBase, musicManager);
        Playlist favoriteMusics = playlistManager.getPlaylist(userPremiumBase.getEmail(), "favorite_musics");

        assertNotNull(favoriteMusics);
        assertEquals("premium.base@example.com", favoriteMusics.getEmail());
        assertEquals("favorite_musics", favoriteMusics.getNome());
        assertFalse(favoriteMusics.isPublica());
        assertTrue(favoriteMusics.getMusicas().isEmpty());
    }

    /**
     * Tests creation of a favorite playlist for a Premium Top user.
     */
    @Test
    public void testCreateFavoriteMusicsForPremiumTopUser() {
        playlistManager.createFavoriteMusicsPlaylist(userPremiumTop, musicManager);
        Playlist favoriteMusics = playlistManager.getPlaylist(userPremiumTop.getEmail(), "favorite_musics");

        assertNotNull(favoriteMusics);
        assertEquals("premium.top@example.com", favoriteMusics.getEmail());
        assertEquals("favorite_musics", favoriteMusics.getNome());
        assertFalse(favoriteMusics.isPublica());
        assertTrue(favoriteMusics.getMusicas().isEmpty());
    }

    /**
     * Tests that Free users cannot create favorite playlists.
     */
    @Test
    public void testCreateFavoriteMusicsForFreeUserThrowsException() {
        Exception exception = assertThrows(IllegalArgumentException.class, () -> {
            new FavoriteMusics(musicasList, userFree);
        });
        assertEquals("Only premium users can have favorite music playlists.", exception.getMessage());
    }

    /**
     * Tests that a newly created favorite playlist is initially empty.
     */
    @Test
    public void testFavoriteMusicsInitiallyEmpty() {
        FavoriteMusics favoriteMusics = new FavoriteMusics(musicasList, userPremiumBase);
        assertNotNull(favoriteMusics.getMusicas());
        assertEquals(0, favoriteMusics.getMusicas().size());
    }

    /**
     * Tests adding a song to the favorite playlist.
     */
    @Test
    public void testAddMusicToFavoriteMusics() {
        FavoriteMusics favoriteMusics = new FavoriteMusics(musicasList, userPremiumBase);
        List<Musica> newMusicas = new ArrayList<>();
        newMusicas.add(musicasList.get(0));
        favoriteMusics.setMusicas(newMusicas);

        List<Musica> musicas = favoriteMusics.getMusicas();
        assertEquals(1, musicas.size());
        assertEquals("Song1", musicas.get(0).getNome());
    }

    /**
     * Tests removing a song from the favorite playlist.
     */
    @Test
    public void testRemoveMusicFromFavoriteMusics() {
        FavoriteMusics favoriteMusics = new FavoriteMusics(musicasList, userPremiumBase);
        favoriteMusics.setMusicas(new ArrayList<>(musicasList.subList(0, 2)));
        assertEquals(2, favoriteMusics.getMusicas().size());

        List<Musica> updatedMusicas = new ArrayList<>();
        updatedMusicas.add(musicasList.get(0));
        favoriteMusics.setMusicas(updatedMusicas);

        List<Musica> musicas = favoriteMusics.getMusicas();
        assertEquals(1, musicas.size());
        assertEquals("Song1", musicas.get(0).getNome());
    }

    /**
     * Tests that a null music list is handled by initializing an empty playlist.
     */
    @Test
    public void testNullMusicasList() {
        FavoriteMusics favoriteMusics = new FavoriteMusics(null, userPremiumBase);
        assertNotNull(favoriteMusics);
        assertTrue(favoriteMusics.getMusicas().isEmpty());
    }
}
