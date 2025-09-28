package spotifum.Users;

import java.time.LocalDateTime;
import java.util.List;

import spotifum.Musics.Musica;
import spotifum.Reproductions.ReproducaoMusical;
import spotifum.Musics.implementations.GeneroMusical;

/**
 * The UtilizadorInterface interface defines the contract for a Spotifum user.
 * It handles user information, subscription plans, music playback, and favorite songs.
 *
 * Authors: Afonso Martins (a106931), Gonçalo Castro (a107337), Luís Felício (a106913)
 * Version: 11052024
 */
public interface UtilizadorInterface {

    /**
     * Gets the user's name.
     */
    String getNome();

    /**
     * Gets the user's email address.
     */
    String getEmail();

    /**
     * Gets the user's address.
     */
    String getMorada();

    /**
     * Gets the user's accumulated points.
     */
    double getPontos();

    /**
     * Gets the user's subscription plan.
     */
    PlanoSubscricao getPlano();

    /**
     * Gets the list of the user's favorite songs.
     */
    List<Musica> getMusicasFavoritas();

    /**
     * Gets the list of the user's music reproductions.
     */
    List<ReproducaoMusical> getReproducaoMusical();

    /**
     * Sets the user's name.
     */
    void setNome(String nome);

    /**
     * Sets the user's email address.
     */
    void setEmail(String email);

    /**
     * Sets the user's address.
     */
    void setMorada(String morada);

    /**
     * Sets the user's subscription plan.
     */
    void setPlano(PlanoSubscricao plano);

    /**
     * Sets the list of reproductions for the user.
     */
    void setReproducoes(List<ReproducaoMusical> reproducoes);

    // Functional methods

    /**
     * Reproduces a music and updates the user's reproduction list and points.
     */
    void reproduzirMusica(String musica, LocalDateTime data);

    /**
     * Adds a music track to the user's list of favorites.
     */
    void addMusicToFavorites(Musica musicaFavorita);

    /**
     * Removes a music track from the user's list of favorites.
     */
    void removeMusicFromFavorites(Musica musicaFavorita);

    /**
     * Returns the user's preferred music genres based on their favorites.
     */
    List<GeneroMusical> getPreferredGenres();

    /**
     * Returns a string representation of the user.
     */
    String toString();

    /**
     * Creates and returns a deep copy of the user.
     */
    Utilizador clone();

    /**
     * Checks equality between users by email.
     */
    boolean equals(Object o);
}