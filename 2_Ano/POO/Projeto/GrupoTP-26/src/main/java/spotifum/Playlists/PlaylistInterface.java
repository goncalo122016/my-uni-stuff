package spotifum.Playlists;

import java.util.List;

import spotifum.Musics.Musica;

/**
 * The PlaylistInterface interface to restrict the implementations connected to playlists.
 * This interface defines the structure for a playlist in the Spotifum system.
 * 
 * @author  Afonso Martins (a106931), Gonçalo Castro (a107337), Luís Felício (a106913)
 * @version (11052024)
 */
public interface PlaylistInterface {

    /**
     * Gets the creator's email address.
     */
    String getEmail();

    /**
     * Gets the name of the playlist.
     */
    String getNome();

    /**
     * Gets the list of music tracks in the playlist.
     */
    List<Musica> getMusicas();

    /**
     * Checks if the playlist is public.
     */
    boolean isPublica();

    /**
     * Gets the number of times the playlist has been played.
     */
    int getNumeroReproducoes();

    /**
     * Sets the creator's email address.
     */
    void setEmail(String email);

    /**
     * Sets the name of the playlist.
     */
    void setNome(String nome);

    /**
     * Sets the list of music tracks in the playlist.
     */
    void setMusicas(List<Musica> musicas);

    /**
     * Sets whether the playlist is public.
     */
    void setPublica(boolean publica);

    /**
     * Sets the number of times the playlist has been played.
     */
    void setNumeroReproducoes(int numeroReproducoes);

    /**
     * Adds a music track to the playlist.
     */
    void addMusica(Musica m);

    /**
     * Removes a music track from the playlist by its name.
     */
    void removeMusica(String name);

    /**
     * Increments the number of times the playlist has been played.
     */
    void reproduzir();

    /**
     * Returns a string representation of the playlist.
     */
    String toString();
}
