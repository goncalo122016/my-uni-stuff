package spotifum.Musics;

import java.io.Serial;
import java.util.ArrayList;
import java.util.List;
import java.io.Serializable;

import spotifum.Musics.implementations.GeneroMusical;

/**
 * Represents a music track in the Spotifum system.
 * This class manages all the information related to a music track, including
 * its metadata, content, and playback statistics.
 *
 * Implements {@link MusicaInterface} and {@link Serializable}.
 */
public class Musica implements MusicaInterface, Serializable {
    @Serial
    private static final long serialVersionUID = 1L;

    /** Title of the music track. */
    private String nome;

    /** Artist or interpreter of the music track. */
    private String interprete;

    /** Name of the music publisher or label. */
    private String nomeEditora;

    /** Lyrics of the music track. */
    private String letra;

    /**
     * Musical content represented as a list of character lines.
     */
    private List<String> musica;

    /** Musical genre of the track. */
    private GeneroMusical generoMusical;

    /** Duration of the track in seconds. */
    private int duracaoSegundos;

    /** Number of times the track has been played. */
    private int numeroReproducoes;

    /**
     * Default constructor for the Musica class.
     * Initializes a music track with default values.
     */
    public Musica() {
        this.nome = "";
        this.interprete = "";
        this.nomeEditora = "";
        this.letra = "";
        this.musica = new ArrayList<>();
        this.generoMusical = GeneroMusical.OUTRO;
        this.duracaoSegundos = 0;
        this.numeroReproducoes = 0;
    }

    /**
     * Copy constructor for the Musica class.
     * Creates a new music track with the same attributes as the provided music track.
     *
     * @param m The music track to copy.
     */
    public Musica(Musica m) {
        this.nome = m.getNome();
        this.interprete = m.getInterprete();
        this.nomeEditora = m.getNomeEditora();
        this.letra = m.getLetra();
        this.musica = m.getMusica();
        this.generoMusical = m.getGeneroMusical();
        this.duracaoSegundos = m.getDuracaoSegundos();
        this.numeroReproducoes = m.getNumeroReproducoes();
    }

    /**
     * Constructor for the Musica class with specific parameters.
     *
     * @param nome            Title of the music track.
     * @param interprete      Artist or interpreter of the music track.
     * @param nomeEditora     Name of the publisher or label.
     * @param letra           Lyrics of the music track.
     * @param musica          Musical content as a list of character lines.
     * @param generoMusical   Musical genre.
     * @param duracaoSegundos Duration of the track in seconds.
     */
    public Musica(String nome, String interprete, String nomeEditora, String letra, List<String> musica,
                  GeneroMusical generoMusical, int duracaoSegundos) {
        this.nome = nome;
        this.interprete = interprete;
        this.nomeEditora = nomeEditora;
        this.letra = letra;
        this.musica = (musica != null) ? new ArrayList<>(musica) : new ArrayList<>();
        this.generoMusical = generoMusical;
        this.duracaoSegundos = duracaoSegundos;
        this.numeroReproducoes = 0;
    }

    /** @return The title of the music track. */
    public String getNome() {
        return nome;
    }

    /** @return The artist or interpreter of the music track. */
    public String getInterprete() {
        return interprete;
    }

    /** @return The name of the publisher or label. */
    public String getNomeEditora() {
        return nomeEditora;
    }

    /** @return The lyrics of the music track. */
    public String getLetra() {
        return letra;
    }

    /** @return The musical content as a list of character lines. */
    public List<String> getMusica() {
        return new ArrayList<>(musica);
    }

    /** @return The musical genre of the track. */
    public GeneroMusical getGeneroMusical() {
        return generoMusical;
    }

    /** @return The duration of the track in seconds. */
    public int getDuracaoSegundos() {
        return duracaoSegundos;
    }

    /** @return The number of times the track has been played. */
    public int getNumeroReproducoes() {
        return numeroReproducoes;
    }

    /** Increments the playback counter by one. */
    public void reproduzir() {
        numeroReproducoes++;
    }

    /**
     * Alias for {@link #getGeneroMusical()}.
     *
     * @return The musical genre of the track.
     */
    public GeneroMusical getGenero() {
        return generoMusical;
    }

    /** @param nome The new title of the music track. */
    public void setNome(String nome) {
        this.nome = nome;
    }

    /** @param interprete The new artist or interpreter. */
    public void setInterprete(String interprete) {
        this.interprete = interprete;
    }

    /** @param nomeEditora The new name of the publisher or label. */
    public void setNomeEditora(String nomeEditora) {
        this.nomeEditora = nomeEditora;
    }

    /** @param letra The new lyrics for the music track. */
    public void setLetra(String letra) {
        this.letra = letra;
    }

    /** @param musica The new musical content. */
    public void setMusica(List<String> musica) {
        if (musica == null) {
            this.musica = new ArrayList<>();
        } else {
            this.musica = new ArrayList<>(musica);
        }
    }

    /**
     * Sets the musical genre from a string.
     * If the string is invalid, the genre is set to OUTRO.
     *
     * @param generoMusical The name of the musical genre.
     */
    public void setGeneroMusical(String generoMusical) {
        try {
            this.generoMusical = GeneroMusical.valueOf(generoMusical.toUpperCase());
        } catch (IllegalArgumentException e) {
            this.generoMusical = GeneroMusical.OUTRO;
        }
    }

    /** @param duracaoSegundos The new duration of the track in seconds. */
    public void setDuracaoSegundos(int duracaoSegundos) {
        this.duracaoSegundos = duracaoSegundos;
    }

    /** @param numeroReproducoes The new number of playbacks. */
    public void setNumeroReproducoes(int numeroReproducoes) {
        this.numeroReproducoes = numeroReproducoes;
    }

    /**
     * Creates and returns a deep copy of this Musica object.
     *
     * @return A new Musica instance with the same attributes.
     */
    @Override
    public Musica clone() {
        return new Musica(this);
    }

    /**
     * Compares this object with another for equality.
     *
     * @param obj The object to compare.
     * @return {@code true} if both objects are equal, {@code false} otherwise.
     */
    @Override
    public boolean equals(Object obj) {
        if (this == obj) return true;
        if (obj == null || getClass() != obj.getClass()) return false;
        Musica other = (Musica) obj;
        return duracaoSegundos == other.duracaoSegundos
                && numeroReproducoes == other.numeroReproducoes
                && nome.equals(other.nome)
                && interprete.equals(other.interprete)
                && nomeEditora.equals(other.nomeEditora)
                && letra.equals(other.letra)
                && musica.equals(other.musica)
                && generoMusical == other.generoMusical;
    }

    /**
     * Returns a string representation of this Musica object.
     *
     * @return A string describing the music track and its attributes.
     */
    @Override
    public String toString() {
        return String.format(
                "{ nome: \"%s\", interprete: \"%s\", editora: \"%s\", genero: \"%s\", duracao: %d, reproducoes: %d, letra: \"%s\", partitura: %s }",
                nome, interprete, nomeEditora, generoMusical, duracaoSegundos, numeroReproducoes,
                letra.isEmpty() ? "N/A" : letra,
                musica != null ? musica.toString() : "[]");
    }
}
