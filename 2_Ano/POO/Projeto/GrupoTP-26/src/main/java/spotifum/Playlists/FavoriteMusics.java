package spotifum.Playlists;

import java.util.ArrayList;
import java.util.List;

import spotifum.Musics.Musica;
import spotifum.Users.PlanoSubscricao;
import spotifum.Users.Utilizador;

/**
 * Represents a specialized playlist for a user's favorite music.
 * This class extends {@link Playlist} and is only available to users with a premium subscription.
 */
public class FavoriteMusics extends Playlist {

    /**
     * Constructs a FavoriteMusics playlist for the specified user based on their subscription and preferences.
     * <p>
     * This playlist is only available to users with a {@code PremiumBase} or {@code PremiumTop} plan.
     * If the user does not meet the criteria, an exception is thrown.
     *
     * @param musicasOriginais A list of available music tracks to select favorites from.
     * @param user             The user for whom the playlist is being created.
     * @throws IllegalArgumentException If the user does not have a premium subscription plan.
     */
    public FavoriteMusics(List<Musica> musicasOriginais, Utilizador user) {
        super(user.getEmail(), "favorite_musics", validateAndGenerateFavorites(musicasOriginais, user), false);
    }

    /**
     * Validates the user's subscription and generates a list of favorite tracks.
     * <p>
     * This method ensures that only users with valid premium subscriptions can have this type of playlist.
     * Currently, it returns an empty list; logic to filter favorites (e.g., by genre, play count) can be added here.
     *
     * @param original The list of all available tracks to consider.
     * @param user     The user whose preferences are used to filter music.
     * @return A list of music tracks considered as favorites.
     * @throws IllegalArgumentException If the user is not a premium subscriber.
     */
    private static List<Musica> validateAndGenerateFavorites(List<Musica> original, Utilizador user) {
        PlanoSubscricao plano = user.getPlano();
        if (plano != PlanoSubscricao.PremiumBase && plano != PlanoSubscricao.PremiumTop) {
            throw new IllegalArgumentException("Only premium users can have favorite music playlists.");
        }

        // Placeholder logic: should be enhanced to select favorites based on user preferences
        return new ArrayList<>();
    }
}
