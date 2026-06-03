package spotifum.Users;

import java.io.FileInputStream;
import java.io.FileOutputStream;
import java.io.IOException;
import java.io.ObjectInputStream;
import java.io.ObjectOutputStream;
import java.time.LocalDateTime;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import spotifum.Exceptions.MusicNotFoundException;
import spotifum.Exceptions.UserAlreadyExistsException;
import spotifum.Exceptions.UserNotFoundException;
import spotifum.Musics.MusicManager;
import spotifum.Musics.Musica;

/**
 * Manages the users of the Spotifum system, including creation,
 * insertion, update, deletion, and interactions with favorite musics
 * and music reproductions.
 */
public class UserManager {
    private final Map<String, Utilizador> utilizadores;

    /**
     * Constructs an empty UserManager.
     */
    public UserManager() {
        this.utilizadores = new HashMap<>();
    }

    /**
     * Constructs a UserManager with an initial map of users.
     *
     * @param utilizadores the map of users to initialize with
     */
    public UserManager(Map<String, Utilizador> utilizadores) {
        this.utilizadores = new HashMap<>(utilizadores);
    }

    /**
     * Creates a new user instance.
     *
     * @param nome   the name of the user
     * @param email  the email of the user
     * @param morada the address of the user
     * @param plano  the subscription plan
     * @return a new Utilizador object
     * @throws IllegalArgumentException if any parameter is null
     * @throws UserAlreadyExistsException if a user with the given email already exists
     */
    public Utilizador createUser(String nome, String email, String morada, PlanoSubscricao plano) {
        if (nome == null || email == null || morada == null || plano == null) {
            throw new IllegalArgumentException("None of the input fields can be null.");
        }

        String key = email.toLowerCase();

        if (existsUser(key)) {
            throw new UserAlreadyExistsException("The user with the email " + email + " already exists.");
        }

        return new Utilizador(nome, email, morada, plano);
    }

    /**
     * Inserts a user into the manager.
     *
     * @param user the user to insert
     * @throws UserAlreadyExistsException if the user already exists
     */
    public void insertUser(Utilizador user) {
        String email = user.getEmail().toLowerCase();
        if (existsUser(email)) {
            throw new UserAlreadyExistsException("The user with the email " + email + " already exists.");
        }
        this.utilizadores.put(email, user);
    }

    /**
     * Updates a user in the manager.
     *
     * @param user the user to update
     * @throws RuntimeException if the user does not exist
     */
    public void updateUser(Utilizador user) throws RuntimeException {
        String email = user.getEmail().toLowerCase();

        if (!existsUser(email)){
            throw new RuntimeException("The user with the email " + email + " does not exist");
        }

        this.utilizadores.put(email, user);
    }

    /**
     * Gets a user by email.
     *
     * @param email the user's email
     * @return the user object
     * @throws UserNotFoundException if the user does not exist
     */
    public Utilizador getUser(String email) {
        String key = email.toLowerCase();
        Utilizador user = this.utilizadores.get(key);
        if (user == null) {
            throw new UserNotFoundException("The user with the email " + email + " does not exist.");
        }
        return user;
    }

    /**
     * Removes a user by email.
     *
     * @param email the user's email
     * @throws UserNotFoundException if the user does not exist
     */
    public void removeUser(String email) {
        String key = email.toLowerCase();
        if (!this.utilizadores.containsKey(key)) {
            throw new UserNotFoundException("The user with the email " + email + " does not exist.");
        }
        this.utilizadores.remove(key);
    }

    /**
     * Checks if a user exists by email.
     *
     * @param email the user's email
     * @return true if the user exists, false otherwise
     */
    public boolean existsUser(String email) {
        return this.utilizadores.containsKey(email.toLowerCase());
    }

    /**
     * Retrieves a list of all users.
     *
     * @return a list of all users
     */
    public List<Utilizador> getAllUsers() {
        return new ArrayList<>(this.utilizadores.values());
    }

    /**
     * Adds a music to a user's favorites.
     *
     * @param musicManager the music manager
     * @param nomeMusica   the name of the music
     * @param emailUser    the user's email
     * @throws MusicNotFoundException if the music does not exist
     * @throws UserNotFoundException  if the user does not exist
     */
    public void addFavoriteMusic(MusicManager musicManager, String nomeMusica, String emailUser) {
        Musica musica = getValidatedMusic(musicManager, nomeMusica);
        Utilizador user = getUser(emailUser);
        user.addMusicToFavorites(musica);
    }

    /**
     * Removes a music from a user's favorites.
     *
     * @param musicManager the music manager
     * @param nomeMusica   the name of the music
     * @param emailUser    the user's email
     * @throws MusicNotFoundException if the music does not exist
     * @throws UserNotFoundException  if the user does not exist
     */
    public void removeFavoriteMusic(MusicManager musicManager, String nomeMusica, String emailUser) {
        Musica musica = getValidatedMusic(musicManager, nomeMusica);
        Utilizador user = getUser(emailUser);
        user.removeMusicFromFavorites(musica);
    }

    /**
     * Registers a music reproduction by a user.
     *
     * @param musicManager the music manager
     * @param nomeMusica   the name of the music
     * @param emailUser    the user's email
     * @param data         the timestamp of the reproduction
     * @throws MusicNotFoundException if the music does not exist
     * @throws UserNotFoundException  if the user does not exist
     */
    public void addMusicReproduction(MusicManager musicManager, String nomeMusica, String emailUser, LocalDateTime data) {
        Musica musica = getValidatedMusic(musicManager, nomeMusica);
        Utilizador user = getUser(emailUser);
        user.reproduzirMusica(nomeMusica, data);
        MusicManager.handleReproduction(musica);
    }

    /**
     * Validates that a music exists and retrieves it.
     *
     * @param manager     the music manager
     * @param nomeMusica  the name of the music
     * @return the music object
     * @throws MusicNotFoundException if the music does not exist
     */
    private Musica getValidatedMusic(MusicManager manager, String nomeMusica) {
        if (!manager.existsMusic(nomeMusica)) {
            throw new MusicNotFoundException("The music " + nomeMusica + " does not exist.");
        }
        return manager.getMusica(nomeMusica);
    }

    /**
     * Saves the current user data to a file.
     *
     * @param filePath the file path to save to
     * @throws IOException if an I/O error occurs
     */
    public void save(String filePath) throws IOException {
        try (ObjectOutputStream out = new ObjectOutputStream(new FileOutputStream(filePath))) {
            out.writeObject(getAllUsers());
        }
    }

    /**
     * Loads user data from a file.
     *
     * @param filePath the file path to load from
     * @throws IOException            if an I/O error occurs
     * @throws ClassNotFoundException if the class of a serialized object cannot be found
     */
    @SuppressWarnings("unchecked")
    public void load(String filePath) throws IOException, ClassNotFoundException {
        List<Utilizador> users;
        try (ObjectInputStream in = new ObjectInputStream(new FileInputStream(filePath))) {
            users = (List<Utilizador>) in.readObject();
        }
        UserManager manager = new UserManager();
        for (Utilizador u : users) {
            manager.insertUser(u);
        }
        this.utilizadores.clear();
        this.utilizadores.putAll(manager.utilizadores);
    }

    /**
     * Creates a deep copy of this UserManager.
     *
     * @return a new UserManager instance with the same users
     */
    public UserManager clone(){
        return new UserManager(this.utilizadores);
    }
}
