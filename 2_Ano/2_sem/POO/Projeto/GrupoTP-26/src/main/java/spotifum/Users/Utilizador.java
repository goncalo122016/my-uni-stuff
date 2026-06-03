package spotifum.Users;

import java.io.Serial;
import java.io.Serializable;
import java.time.LocalDateTime;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import spotifum.Musics.Musica;
import spotifum.Musics.implementations.GeneroMusical;
import spotifum.Reproductions.ReproducaoMusical;

/**
 * Represents a user in the Spotifum platform.
 * A user has personal details, a subscription plan, a list of favorite songs,
 * a record of song reproductions, and a point balance.
 */
public class Utilizador implements UtilizadorInterface, Serializable {

    @Serial
    private static final long serialVersionUID = 1L;

    private String nome;
    private String email;
    private String morada;
    private double pontos;
    private final List<Musica> musicasFavoritas;
    private PlanoSubscricao plano;
    private List<ReproducaoMusical> reproducoes;

    /**
     * Default constructor.
     * Initializes a user with empty details, a FREE plan, zero points,
     * and empty lists for favorite songs and reproductions.
     */
    public Utilizador() {
        this.nome = "";
        this.email = "";
        this.morada = "";
        this.plano = PlanoSubscricao.FREE;
        this.pontos = 0;
        this.musicasFavoritas = new ArrayList<>();
        this.reproducoes = new ArrayList<>();
    }

    /**
     * Copy constructor.
     * Creates a deep copy of the given user.
     *
     * @param u The user to copy.
     */
    public Utilizador(Utilizador u) {
        this.nome = u.getNome();
        this.email = u.getEmail();
        this.morada = u.getMorada();
        this.plano = u.getPlano();
        this.pontos = u.getPontos();
        this.musicasFavoritas = new ArrayList<>();
        for (Musica m : u.getMusicasFavoritas()) {
            this.musicasFavoritas.add(m.clone());
        }
        this.reproducoes = new ArrayList<>();
        for (ReproducaoMusical r : u.getReproducaoMusical()) {
            this.reproducoes.add(r.clone());
        }
    }

    /**
     * Parameterized constructor.
     * Creates a user with the provided details and initializes other fields accordingly.
     *
     * @param nome   The user's name.
     * @param email  The user's email.
     * @param morada The user's address.
     * @param plano  The user's subscription plan.
     */
    public Utilizador(String nome, String email, String morada, PlanoSubscricao plano) {
        this.nome = nome;
        this.email = email;
        this.morada = morada;
        this.plano = plano;
        this.pontos = (plano == PlanoSubscricao.PremiumTop) ? 100 : 0;
        this.musicasFavoritas = new ArrayList<>();
        this.reproducoes = new ArrayList<>();
    }

    /**
     * Registers the reproduction of a song by the user.
     * Updates user points and adds the reproduction record.
     *
     * @param musica The name of the song.
     * @param data   The reproduction timestamp.
     */
    public void reproduzirMusica(String musica, LocalDateTime data) {
        this.pontos = plano.calcularPontos(this.pontos);
        this.reproducoes.add(new ReproducaoMusical(musica, data, this.email));
    }

    public String getNome() {
        return nome;
    }

    public void setNome(String nome) {
        this.nome = nome;
    }

    public String getEmail() {
        return email;
    }

    public void setEmail(String email) {
        this.email = email;
    }

    public String getMorada() {
        return morada;
    }

    public void setMorada(String morada) {
        this.morada = morada;
    }

    public double getPontos() {
        return pontos;
    }

    public void setPontos(double pontos) {
        this.pontos = pontos;
    }

    /**
     * Returns a copy of the user's favorite songs.
     *
     * @return A list of favorite songs.
     */
    public List<Musica> getMusicasFavoritas() {
        return new ArrayList<>(this.musicasFavoritas);
    }

    /**
     * Returns a copy of the user's song reproductions.
     *
     * @return A list of reproductions.
     */
    public List<ReproducaoMusical> getReproducaoMusical() {
        return new ArrayList<>(this.reproducoes);
    }

    public PlanoSubscricao getPlano() {
        return plano;
    }

    public void setPlano(PlanoSubscricao plano) {
        this.plano = plano;
    }

    /**
     * Replaces the user's reproduction history with a new list.
     *
     * @param reproducoes The new list of reproductions.
     */
    public void setReproducoes(List<ReproducaoMusical> reproducoes) {
        this.reproducoes = new ArrayList<>(reproducoes);
    }

    /**
     * Adds a song to the user's favorites.
     *
     * @param musicaFavorita The song to add.
     * @throws RuntimeException if the song is already in favorites.
     */
    public void addMusicToFavorites(Musica musicaFavorita) throws RuntimeException {
        if (this.musicasFavoritas.contains(musicaFavorita)) {
            throw new RuntimeException("This music is already in the favorites.");
        }
        this.musicasFavoritas.add(musicaFavorita);
    }

    /**
     * Removes a song from the user's favorites.
     *
     * @param musicaFavorita The song to remove.
     * @throws RuntimeException if the song is not in favorites.
     */
    public void removeMusicFromFavorites(Musica musicaFavorita) {
        if (!this.musicasFavoritas.contains(musicaFavorita)) {
            throw new RuntimeException("This music is not in the favorites.");
        }
        this.musicasFavoritas.remove(musicaFavorita);
    }

    /**
     * Analyzes the user's favorite songs and returns the most frequent genres.
     *
     * @return A list of the user's preferred musical genres.
     */
    public List<GeneroMusical> getPreferredGenres() {
        Map<GeneroMusical, Integer> genreCount = new HashMap<>();
        for (Musica musica : musicasFavoritas) {
            GeneroMusical genero = musica.getGenero();
            genreCount.put(genero, genreCount.getOrDefault(genero, 0) + 1);
        }

        List<GeneroMusical> preferredGenres = new ArrayList<>();
        int maxCount = 0;
        for (Map.Entry<GeneroMusical, Integer> entry : genreCount.entrySet()) {
            if (entry.getValue() > maxCount) {
                preferredGenres.clear();
                preferredGenres.add(entry.getKey());
                maxCount = entry.getValue();
            } else if (entry.getValue() == maxCount) {
                preferredGenres.add(entry.getKey());
            }
        }

        return preferredGenres;
    }

    /**
     * Creates and returns a deep copy of this Utilizador object.
     *
     * @return A new Utilizador instance with the same attributes.
     */
    @Override
    public Utilizador clone() {
        return new Utilizador(this);
    }

    /**
     * Compares this object with another for equality.
     *
     * @param o The object to compare.
     * @return {@code true} if both objects are equal, {@code false} otherwise.
     */
    @Override
    public boolean equals(Object o) {
        if (this == o) return true;
        if (!(o instanceof Utilizador)) return false;
        Utilizador that = (Utilizador) o;
        return email.equals(that.email);
    }

    /**
     * Returns a string representation of this Utilizador object.
     *
     * @return A string describing the user and its attributes.
     */
    @Override
    public String toString() {
        return String.format(
                "{ nome: \"%s\", email: \"%s\", morada: \"%s\", pontos: %.2f, plano: \"%s\", musicasFavoritas: %s, reproducoes: %s }",
                nome, email, morada, pontos, plano,
                musicasFavoritas.stream().map(m -> "\"" + m.getNome() + "\"").toList(),
                reproducoes.stream().map(ReproducaoMusical::toString).toList());
    }
}
