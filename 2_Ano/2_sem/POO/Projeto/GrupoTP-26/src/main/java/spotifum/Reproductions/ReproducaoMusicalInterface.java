package spotifum.Reproductions;

import java.time.LocalDateTime;

/**
 * The ReproducaoMusicalInterface defines the contract for music playback records in the Spotifum system.
 * It includes information such as the song name, playback timestamp, and the user email who played it.
 * 
 * Authors: Afonso Martins (a106931), Gonçalo Castro (a107337), Luís Felício (a106913)
 * Version: 11052024
 */
public interface ReproducaoMusicalInterface {

    /**
     * Gets the name of the music that was played.
     */
    String getMusica();

    /**
     * Gets the date and time the music was played.
     */
    LocalDateTime getDataReproducao();

    /**
     * Gets the email of the user who played the music.
     */
    String getEmail();

    /**
     * Sets the name of the music that was played.
     */
    void setMusica(String musica);

    /**
     * Sets the date and time the music was played.
     */
    void setDataReproducao(LocalDateTime data);

    /**
     * Sets the email of the user who played the music.
     */
    void setEmail(String email);

    /**
     * Returns a copy of this music playback record.
     */
    ReproducaoMusical clone();

    /**
     * Checks whether two playback records are equal.
     */
    boolean equals(Object obj);

    /**
     * Returns a string representation of the playback record.
     */
    String toString();
}
