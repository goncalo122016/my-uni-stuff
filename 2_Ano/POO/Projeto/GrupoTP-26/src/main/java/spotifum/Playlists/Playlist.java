package spotifum.Playlists;

import java.io.Serial;
import java.io.Serializable;
import java.util.ArrayList;
import java.util.List;

import spotifum.Musics.Musica;

/**
 * Represents a playlist in the Spotifum system.
 * This class manages collections of music tracks that can be organized and shared.
 */
public class Playlist implements PlaylistInterface, Serializable {
    @Serial
    private static final long serialVersionUID = 1L;

    private String email;
    private String nome;
    private List<Musica> musicas;
    private boolean publica;
    private int numeroReproducoes;

    /**
     * Default constructor for the Playlist class.
     * Initializes a playlist with default values.
     */
    public Playlist() {
        this.email = "";
        this.nome = "";
        this.musicas = new ArrayList<>();
        this.publica = false;
        this.numeroReproducoes = 0;
    }

    /**
     * Copy constructor for the Playlist class.
     * Creates a new playlist with the same attributes as the provided playlist.
     *
     * @param p The playlist to copy.
     */
    public Playlist(Playlist p) {
        this.email = p.getEmail();
        this.nome = p.getNome();
        this.musicas = p.getMusicas();
        this.publica = p.isPublica();
        this.numeroReproducoes = p.getNumeroReproducoes();
    }

    /**
     * Constructor for the Playlist class with specific parameters.
     *
     * @param email    The email of the creator.
     * @param nome     The name of the playlist.
     * @param musicas  The list of music tracks in the playlist.
     * @param publica  Whether the playlist is public.
     */
    public Playlist(String email, String nome, List<Musica> musicas, boolean publica) {
        this.email = email;
        this.nome = nome;
        this.musicas = (musicas != null) ? new ArrayList<>(musicas) : new ArrayList<>();
        this.publica = publica;
        this.numeroReproducoes = 0;
    }

    public String getEmail() {
        return email;
    }

    /**
     * Gets the name of the playlist.
     *
     * @return The name of the playlist.
     */
    public String getNome() {
        return nome;
    }

    /**
     * Gets the list of music tracks in the playlist.
     *
     * @return The list of music tracks in the playlist.
     */
    public List<Musica> getMusicas() {
        return new ArrayList<>(musicas);
    }

    /**
     * Checks if the playlist is public.
     *
     * @return True if the playlist is public, false otherwise.
     */
    public boolean isPublica() {
        return publica;
    }

    /**
     * Gets the number of times the playlist has been played.
     *
     * @return The number of playbacks.
     */
    public int getNumeroReproducoes() {
        return numeroReproducoes;
    }

    public void setEmail(String email) {
        this.email = email;
    }

    /**
     * Sets the name of the playlist.
     *
     * @param nome The new name for the playlist.
     */
    public void setNome(String nome) {
        this.nome = nome;
    }

    /**
     * Sets the list of music tracks in the playlist.
     *
     * @param musicas The new list of music tracks for the playlist.
     */
    public void setMusicas(List<Musica> musicas) {
        this.musicas = new ArrayList<>(musicas);
    }

    /**
     * Sets whether the playlist is public.
     *
     * @param publica True to make the playlist public, false to make it private.
     */
    public void setPublica(boolean publica) {
        this.publica = publica;
    }

    /**
     * Sets the number of times the playlist has been played.
     *
     * @param numeroReproducoes The new number of playbacks.
     */
    public void setNumeroReproducoes(int numeroReproducoes) {
        this.numeroReproducoes = numeroReproducoes;
    }

    /**
     * Adds a music track to the playlist.
     *
     * @param m The music track to add.
     */
    public void addMusica(Musica m) {
        this.musicas.add(m);
    }

    /**
     * Removes a music track from the playlist by its name.
     *
     * @param name The name of the music track to remove.
     */
    public void removeMusica(String name) {
        this.musicas.removeIf(music -> music.getNome().equals(name));
    }

    @Override
    public Playlist clone() {
        return new Playlist(this);
    }

    @Override
    public boolean equals(Object o) {
        if (this == o) return true;
        if (o == null || getClass() != o.getClass()) return false;

        Playlist playlist = (Playlist) o;

        return this.email.equals(playlist.email) &&
                this.nome.equals(playlist.nome) &&
                this.publica == playlist.publica &&
                this.numeroReproducoes == playlist.numeroReproducoes &&
                this.musicas.equals(playlist.musicas);
    }

    @Override
    public String toString() {
        return String.format("{ nome: \"%s\", criador: \"%s\", publica: %b, reproducoes: %d, musicas: %s }",
                nome, email, publica, numeroReproducoes,
                musicas.stream().map(m -> "\"" + m.getNome() + "\"").toList());
    }

    /**
     * Increments the number of times the playlist has been played.
     */
    public void reproduzir() {
        this.numeroReproducoes++;
    }
}
