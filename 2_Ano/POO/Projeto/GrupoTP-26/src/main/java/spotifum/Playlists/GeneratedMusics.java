package spotifum.Playlists;

import java.util.ArrayList;
import java.util.List;
import java.util.stream.Collectors;
import java.util.ArrayList;

import spotifum.Musics.Musica;
import spotifum.Musics.implementations.Explicita;
import spotifum.Users.PlanoSubscricao;
import spotifum.Users.Utilizador;

/**
 * Represents a dynamically generated playlist based on a user's preferences.
 * <p>
 * Only users with a premium subscription plan ({@code PremiumBase} or {@code PremiumTop})
 * can access this type of playlist. Generation can be based on genre preferences,
 * duration constraints, or explicit content.
 */
public class GeneratedMusics extends Playlist {

    /**
     * Constructs a GeneratedMusics playlist for a user based on specific filtering criteria.
     *
     * @param musicasOriginais   The list of available music to filter from.
     * @param user               The user for whom the playlist is generated.
     * @param maxDurationSeconds The maximum total duration in seconds of the playlist (0 for no limit).
     * @param explicit           Whether the playlist should include only explicit content.
     * @throws IllegalArgumentException If the user is not a premium subscriber.
     */
    public GeneratedMusics(List<Musica> musicasOriginais, Utilizador user, int maxDurationSeconds, boolean explicit) {
        super(user.getEmail(), "generated_musics",
                determinePlaylist(musicasOriginais, user, maxDurationSeconds, explicit), false);
    }

    /**
     * Updates the current playlist with a new list of music based on the user's preferences.
     * <p>
     * This method uses genre preferences to filter music.
     *
     * @param musics The list of music to filter.
     * @param user   The user whose preferences are used.
     * @param maxDurationSeconds Maximum duration constraint (not currently applied in this method).
     * @param explicit Whether to include only explicit music (not currently applied in this method).
     */
    public void updateGeneratedMusics(List<Musica> musics, Utilizador user) {
        this.setMusicas(generateFavoriteMusics(musics, user));
    }

    /**
     * Filters music based on the user's preferred genres.
     *
     * @param musics The music to filter.
     * @param user   The user whose genre preferences are considered.
     * @return A list of music matching the user's genres.
     */
    private static List<Musica> generateFavoriteMusics(List<Musica> musics, Utilizador user) {
        return musics.stream()
                .filter(music -> user.getPreferredGenres().contains(music.getGenero()))
                .collect(Collectors.toList());
    }

    /**
     * Filters music based on the user's preferred genres and a maximum duration constraint.
     *
     * @param musics             The list of music to filter.
     * @param user               The user whose preferences are used.
     * @param maxDurationSeconds The maximum total duration in seconds.
     * @return A list of music matching the genre and duration criteria.
     */
    private static List<Musica> generateFavoriteMusicsWithDuration(List<Musica> musics, Utilizador user, int maxDurationSeconds) {
        List<Musica> filteredMusics = new ArrayList<>();
        int currentDuration = 0;

        List<Musica> preferredMusics = musics.stream()
                .filter(music -> user.getPreferredGenres().contains(music.getGenero()))
                .collect(Collectors.toList());

        for (Musica music : preferredMusics) {
            if (currentDuration + music.getDuracaoSegundos() <= maxDurationSeconds) {
                filteredMusics.add(music);
                currentDuration += music.getDuracaoSegundos();
            }
        }

        return filteredMusics;
    }

    /**
     * Filters music based on the user's preferred genres and explicit content.
     *
     * @param musics The list of music to filter.
     * @param user   The user whose preferences are used.
     * @return A list of explicit music matching the user's genres.
     */
    private static List<Musica> generateFavoriteExplicitMusics(List<Musica> musics, Utilizador user) {
        return musics.stream()
                .filter(music -> user.getPreferredGenres().contains(music.getGenero()))
                .filter(music -> music instanceof Explicita)
                .collect(Collectors.toList());
    }

    /**
     * Determines which playlist generation method to use based on the provided parameters.
     *
     * @param musics             The original list of music to filter.
     * @param user               The user whose preferences will guide the filtering.
     * @param maxDurationSeconds The maximum total duration in seconds.
     * @param explicit           Whether to restrict to explicit content.
     * @return A filtered list of music based on the user's preferences.
     * @throws IllegalArgumentException If the user is not a premium subscriber.
     */
    private static List<Musica> determinePlaylist(List<Musica> musics, Utilizador user, int maxDurationSeconds, boolean explicit) {
        if (user.getPlano() != PlanoSubscricao.PremiumBase && user.getPlano() != PlanoSubscricao.PremiumTop) {
            throw new IllegalArgumentException("Only premium users can have generated music playlists.");
        }

        if (maxDurationSeconds > 0) {
            return generateFavoriteMusicsWithDuration(musics, user, maxDurationSeconds);
        } else if (explicit) {
            return generateFavoriteExplicitMusics(musics, user);
        } else {
            return generateFavoriteMusics(musics, user);
        }
    }
}
