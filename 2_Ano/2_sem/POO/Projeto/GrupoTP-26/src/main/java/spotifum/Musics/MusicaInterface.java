package spotifum.Musics;

import spotifum.Musics.implementations.GeneroMusical;
import java.util.List;

/**
 * The MusicaInterface interface restricts the behavior of music track implementations in Spotifum.
 * It defines the methods for managing metadata, playback statistics, and musical content.
 * 
 * @author  Afonso Martins (a106931), Gonçalo Castro (a107337), Luís Felício (a106913) 
 * @version (11052024)
 */
public interface MusicaInterface {

    /**
     * Gets the name of the music track.
     */
    String getNome();

    /**
     * Gets the artist/interpreter of the music track.
     */
    String getInterprete();

    /**
     * Gets the name of the publisher/label.
     */
    String getNomeEditora();

    /**
     * Gets the lyrics of the music track.
     */
    String getLetra();

    /**
     * Gets the collection of musical character lines.
     */
    List<String> getMusica();

    /**
     * Gets the musical genre of the track.
     */
    GeneroMusical getGeneroMusical();

    /**
     * Gets the duration of the track in seconds.
     */
    int getDuracaoSegundos();

    /**
     * Gets the number of times the track has been played.
     */
    int getNumeroReproducoes();

    /**
     * Increments the number of reproductions of the music track.
     */
    void reproduzir();

    /**
     * Gets the musical genre of the track (alias for getGeneroMusical).
     */
    GeneroMusical getGenero();

    /**
     * Sets the name of the music track.
     */
    void setNome(String nome);

    /**
     * Sets the artist/interpreter of the music track.
     */
    void setInterprete(String interprete);

    /**
     * Sets the name of the publisher/label.
     */
    void setNomeEditora(String nomeEditora);

    /**
     * Sets the lyrics of the music track.
     */
    void setLetra(String letra);

    /**
     * Sets the collection of musical character lines.
     */
    void setMusica(List<String> musica);

    /**
     * Sets the musical genre of the track based on a string value.
     */
    void setGeneroMusical(String generoMusical);

    /**
     * Sets the duration of the track in seconds.
     */
    void setDuracaoSegundos(int duracaoSegundos);

    /**
     * Sets the number of times the track has been played.
     */
    void setNumeroReproducoes(int numeroReproducoes);

    /**
     * Creates and returns a copy of this music track.
     */
    MusicaInterface clone();

    /**
     * Checks if this music track is equal to another object.
     */
    boolean equals(Object obj);

    /**
     * Returns the string representation of the music track.
     */
    String toString();
}

