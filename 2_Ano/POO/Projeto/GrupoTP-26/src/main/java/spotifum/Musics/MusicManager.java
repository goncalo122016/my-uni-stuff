package spotifum.Musics;

import java.io.FileInputStream;
import java.io.FileOutputStream;
import java.io.IOException;
import java.io.ObjectInputStream;
import java.io.ObjectOutputStream;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import spotifum.Musics.implementations.Explicita;
import spotifum.Musics.implementations.GeneroMusical;
import spotifum.Musics.implementations.Multimedia;
import spotifum.Musics.implementations.TipoMusica;

/**
 * Manages a collection of music tracks in the Spotifum system.
 * Provides functionality to create, insert, update, retrieve, remove, and serialize music data.
 */
public class MusicManager {

    private final Map<String, Musica> musicas;

    /**
     * Default constructor.
     * Initializes the manager with an empty music collection.
     */
    public MusicManager(){
        this.musicas = new HashMap<>();
    }

    /**
     * Constructs a MusicManager with an existing collection of music tracks.
     *
     * @param musicas A map of music tracks keyed by their lowercase name.
     */
    public MusicManager(Map<String, Musica> musicas){
        this.musicas = new HashMap<>(musicas);
    }

    /**
     * Creates a new music track instance of a specific type.
     *
     * @param nome            The name of the track.
     * @param interprete      The performer or artist.
     * @param nomeEditora     The name of the publisher or label.
     * @param letra           The lyrics of the track.
     * @param musica          A list of strings representing the musical notation.
     * @param generoMusical   The genre of the track.
     * @param duracaoSegundos Duration of the track in seconds.
     * @param video           Video URL or identifier (for multimedia tracks).
     * @param type            Type of the track (NORMAL, MULTIMEDIA, EXPLICITA).
     * @return A new instance of Musica or one of its subclasses.
     * @throws RuntimeException If the type is not recognized.
     */
    public Musica createMusica(String nome, String interprete, String nomeEditora, String letra, List<String> musica,
                               GeneroMusical generoMusical, int duracaoSegundos, String video, TipoMusica type) throws RuntimeException {

        Musica nova;

        switch (type) {
            case MULTIMEDIA:
                nova = new Multimedia(nome.trim(), interprete.trim(), nomeEditora.trim(), letra, musica, generoMusical, duracaoSegundos,
                        video);
                break;
            case NORMAL:
                nova = new Musica(nome.trim(), interprete.trim(), nomeEditora.trim(), letra, musica, generoMusical, duracaoSegundos);
                break;
            case EXPLICITA:
                nova = new Explicita(nome.trim(), interprete.trim(), nomeEditora.trim(), letra, musica, generoMusical, duracaoSegundos);
                break;
            default:
                throw new RuntimeException("Not a valid music type.");
        }
        return nova;
    }

    /**
     * Inserts a new music track into the collection.
     *
     * @param musica The music track to insert.
     * @throws RuntimeException If a track with the same name already exists.
     */
    public void insertMusica(Musica musica) throws RuntimeException {
        String name = musica.getNome().toLowerCase();

        if (existsMusic(name)) {
            throw new RuntimeException("The music with the name " + name + " already exists.");
        }

        this.musicas.put(name, musica);
    }

    /**
     * Updates an existing music track in the collection.
     *
     * @param musica The music track to update.
     * @throws RuntimeException If the track does not exist.
     */
    public void updateMusica(Musica musica) throws RuntimeException {
        String name = musica.getNome().toLowerCase();

        if (!existsMusic(name)) {
            throw new RuntimeException("The music with the name " + name + " does not exist.");
        }

        this.musicas.put(name, musica);
    }

    /**
     * Handles the reproduction of a track by incrementing its play count.
     *
     * @param musica The music track to play.
     */
    public static void handleReproduction(Musica musica) throws RuntimeException {
        musica.reproduzir();
    }

    /**
     * Retrieves a music track by its name.
     *
     * @param name The name of the track.
     * @return The corresponding music track.
     * @throws RuntimeException If the track does not exist.
     */
    public Musica getMusica(String name) throws RuntimeException{
        Musica musica = this.musicas.get(name.toLowerCase().trim());

        if (musica == null) {
            throw new RuntimeException("Music with name " + name + " does not exist.");
        }
        return musica;
    }

    /**
     * Checks if a music track exists in the collection.
     *
     * @param name The name of the track.
     * @return {@code true} if it exists, {@code false} otherwise.
     */
    public boolean existsMusic(String name){
        return this.musicas.containsKey(name.toLowerCase().trim());
    }

    /**
     * Removes a music track from the collection by its name.
     *
     * @param name The name of the track to remove.
     * @throws RuntimeException If the track does not exist.
     */
    public void removeMusic(String name) throws RuntimeException {
        if (!existsMusic(name)) {
            throw new RuntimeException("Music with name " + name + " does not exist.");
        }

        this.musicas.remove(name.toLowerCase());
    }

    /**
     * Retrieves all music tracks managed by this instance.
     *
     * @return A list of all music tracks.
     */
    public List<Musica> getAllMusics() {
        return new ArrayList<>(this.musicas.values());
    }

    /**
     * Saves the current music collection to a file.
     *
     * @param filePath The path to the file where the data will be stored.
     * @throws IOException If an I/O error occurs.
     */
    public void save(String filePath) throws IOException {
        try (ObjectOutputStream out = new ObjectOutputStream(new FileOutputStream(filePath))) {
            out.writeObject(getAllMusics());
        }
    }

    /**
     * Loads music tracks from a file and replaces the current collection.
     *
     * @param filePath The path to the file containing serialized music data.
     * @throws IOException If an I/O error occurs.
     * @throws ClassNotFoundException If the serialized data is invalid or incompatible.
     */
    @SuppressWarnings("unchecked")
    public void load(String filePath) throws IOException, ClassNotFoundException {
        List<Musica> musicas;
        try (ObjectInputStream in = new ObjectInputStream(new FileInputStream(filePath))) {
            musicas = (List<Musica>) in.readObject();
        }
        MusicManager manager = new MusicManager();
        for (Musica m : musicas) {
            manager.insertMusica(m);
        }
        this.musicas.clear();
        this.musicas.putAll(manager.musicas);
    }

    /**
     * Creates and returns a copy of this {@code MusicManager}.
     *
     * @return A new instance with a duplicated music collection.
     */
    public MusicManager clone() {
        return new MusicManager(this.musicas);
    }
}
