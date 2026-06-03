package spotifum.Playlists;

import java.io.*;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import spotifum.Musics.MusicManager;
import spotifum.Musics.Musica;
import spotifum.Users.UserManager;
import spotifum.Users.Utilizador;

/**
 * Manages playlists for users, including creation, insertion, retrieval,
 * updates, and deletions. It supports public playlists and automatic/favorite playlists.
 */
public class PlaylistManager {

    private final Map<String, Map<String, Playlist>> playlists;

    /**
     * Constructs a new PlaylistManager.
     */
    public PlaylistManager() {
        this.playlists = new HashMap<>();
    }

    /**
     * Creates a new playlist for a user.
     *
     * @param managerUser  User manager to validate user existence.
     * @param managerMusic Music manager to retrieve songs.
     * @param email        User's email.
     * @param musicas      List of music names.
     * @param nome         Playlist name.
     * @param publica      Whether the playlist is public.
     * @return The created Playlist object.
     * @throws RuntimeException if the user does not exist.
     */
    public Playlist createPlaylist(UserManager managerUser, MusicManager managerMusic, String email,
                                   List<String> musicas, String nome, boolean publica) throws RuntimeException {

        if (!managerUser.existsUser(email)) {
            throw new RuntimeException("User does not exist.");
        }

        List<Musica> musics = new ArrayList<>();
        List<String> musicsNotAdded = new ArrayList<>();

        for (String musica : musicas) {
            if (!managerMusic.existsMusic(musica)) {
                musicsNotAdded.add(musica);
            }
            musics.add(managerMusic.getMusica(musica));
        }

        if (!musicsNotAdded.isEmpty()) {
            System.out.println(musicsNotAdded + "\n These songs do not exist.\n");
        }

        return new Playlist(email, nome, musics, publica);
    }

    /**
     * Inserts a playlist into the user's collection.
     *
     * @param playlist The playlist to be inserted.
     */
    public void insertPlaylist(Playlist playlist) {
        String email = playlist.getEmail().toLowerCase();
        String playlistName = playlist.getNome().toLowerCase();

        Map<String, Playlist> userPlaylists = playlists.getOrDefault(email, new HashMap<>());
        userPlaylists.put(playlistName, playlist.clone());
        playlists.put(email, userPlaylists);
    }

    /**
     * Retrieves a playlist by email and name, including public playlists.
     *
     * @param email User email.
     * @param nome  Playlist name.
     * @return The Playlist object, or null if not found.
     */
    public Playlist getPlaylist(String email, String nome) {
        Map<String, Playlist> userPlaylists = this.playlists.get(email.toLowerCase());

        if (userPlaylists != null) {
            Playlist p = userPlaylists.get(nome.toLowerCase());
            if (p != null) return p.clone();
        }

        String nomeLower = nome.toLowerCase();
        for (Map.Entry<String, Map<String, Playlist>> userEntry : this.playlists.entrySet()) {
            for (Map.Entry<String, Playlist> playlistEntry : userEntry.getValue().entrySet()) {
                Playlist p = playlistEntry.getValue();
                if (p.isPublica() && p.getNome().toLowerCase().equals(nomeLower)) {
                    return p.clone();
                }
            }
        }

        return null;
    }

    /**
     * Increments the play count of a specific playlist.
     *
     * @param email User email.
     * @param nome  Playlist name.
     */
    public void incrementNReproducoes(String email, String nome) {
        Map<String, Playlist> userPlaylists = this.playlists.get(email.toLowerCase());
        String nomeLower = nome.toLowerCase();

        if (userPlaylists != null) {
            Playlist p = userPlaylists.get(nomeLower);
            if (p != null) {
                p.setNumeroReproducoes(p.getNumeroReproducoes() + 1);
                return;
            }
        }

        for (Map<String, Playlist> playlists : this.playlists.values()) {
            Playlist p = playlists.get(nomeLower);
            if (p != null && p.isPublica()) {
                p.setNumeroReproducoes(p.getNumeroReproducoes() + 1);
                return;
            }
        }
    }

    /**
     * Retrieves all playlists of a specific user.
     *
     * @param email User email.
     * @return A map of playlist names to Playlist objects.
     */
    public Map<String, Playlist> getAllPlaylistUser(String email) {
        Map<String, Playlist> original = this.playlists.get(email.toLowerCase());
        if (original == null) return null;

        Map<String, Playlist> copia = new HashMap<>();
        for (Map.Entry<String, Playlist> entry : original.entrySet()) {
            copia.put(entry.getKey(), entry.getValue().clone());
        }
        return copia;
    }

    /**
     * Removes a playlist from a user's collection.
     *
     * @param managerUser User manager to validate user existence.
     * @param email       User email.
     * @param nome        Playlist name.
     * @throws RuntimeException if user or playlist does not exist.
     */
    public void removePlaylist(UserManager managerUser, String email, String nome) {
        Map<String, Playlist> userPlaylists = playlists.get(email.toLowerCase());

        if (!managerUser.existsUser(email)) {
            throw new RuntimeException("O utilizador não existe!");
        }

        if (userPlaylists == null || !userPlaylists.containsKey(nome.toLowerCase())) {
            throw new RuntimeException("A playlist não existe ou não pertence ao utilizador.");
        }

        userPlaylists.remove(nome.toLowerCase());
    }

    /**
     * Checks if a playlist exists, including public ones.
     *
     * @param email User email.
     * @param nome  Playlist name.
     * @return True if the playlist exists, false otherwise.
     */
    public boolean existsPlaylist(String email, String nome) {
        Map<String, Playlist> userPlaylists = this.playlists.get(email.toLowerCase());
        if (userPlaylists != null && userPlaylists.containsKey(nome.toLowerCase())) {
            return true;
        }

        String nomeLower = nome.toLowerCase();
        for (Map.Entry<String, Map<String, Playlist>> userEntry : this.playlists.entrySet()) {
            for (Map.Entry<String, Playlist> playlistEntry : userEntry.getValue().entrySet()) {
                Playlist p = playlistEntry.getValue();
                if (p.isPublica() && p.getNome().toLowerCase().equals(nomeLower)) {
                    return true;
                }
            }
        }

        return false;
    }

    /**
     * Retrieves all playlists from all users.
     *
     * @return A list of all playlists.
     */
    public List<Playlist> getAllPlaylists() {
        List<Playlist> allPlaylists = new ArrayList<>();
        for (Map<String, Playlist> userPlaylists : playlists.values()) {
            for (Playlist p : userPlaylists.values()) {
                allPlaylists.add(p.clone());
            }
        }
        return allPlaylists;
    }

    /**
     * Returns a deep copy of all playlists.
     *
     * @return Map of user emails to their playlists.
     */
    public Map<String, Map<String, Playlist>> getPlaylists() {
        Map<String, Map<String, Playlist>> copia = new HashMap<>();

        for (Map.Entry<String, Map<String, Playlist>> entry : playlists.entrySet()) {
            Map<String, Playlist> userPlaylistsClone = new HashMap<>();
            for (Map.Entry<String, Playlist> playlistEntry : entry.getValue().entrySet()) {
                userPlaylistsClone.put(playlistEntry.getKey(), playlistEntry.getValue().clone());
            }
            copia.put(entry.getKey(), userPlaylistsClone);
        }

        return copia;
    }

    /**
     * Creates a generated playlist based on user preferences.
     *
     * @param user       The user.
     * @param userMusics List of music options.
     * @param duration   Desired duration in minutes.
     * @param explicit   Whether explicit content is allowed.
     */
    public void createGeneratedMusicsPlaylist(Utilizador user, List<Musica> userMusics, int duration, boolean explicit) {
        try {
            GeneratedMusics generatedPlaylist = new GeneratedMusics(userMusics, user, duration, explicit);
            insertPlaylist(generatedPlaylist);
        } catch (IllegalArgumentException e) {
            throw new RuntimeException("Could not create generated playlist: " + e.getMessage());
        }
    }

    /**
     * Updates the generated playlist for the user.
     *
     * @param user       The user.
     * @param userMusics List of music options.
     * @param duration   Desired duration.
     * @param explicit   Whether explicit content is allowed.
     */
    public void updateGeneratedMusicsPlaylist(Utilizador user, List<Musica> userMusics, int duration, boolean explicit) {
        try {
            Map<String, Playlist> userPlaylists = playlists.get(user.getEmail().toLowerCase());
            if (userPlaylists == null) {
                throw new RuntimeException("User not found");
            }

            Playlist playlist = userPlaylists.get("generated_musics");
            if (playlist == null) {
                throw new RuntimeException("Generated playlist not found");
            }

            GeneratedMusics newGeneratedPlaylist = new GeneratedMusics(userMusics, user, duration, explicit);
            userPlaylists.put("generated_musics", newGeneratedPlaylist);
        } catch (IllegalArgumentException e) {
            throw new RuntimeException("Could not update generated playlist: " + e.getMessage());
        }
    }

    /**
     * Creates a favorite songs playlist for the user.
     *
     * @param user         The user.
     * @param musicManager Music manager to fetch songs.
     */
    public void createFavoriteMusicsPlaylist(Utilizador user, MusicManager musicManager) {
        try {
            List<Musica> allMusics = musicManager.getAllMusics();
            FavoriteMusics favoritePlaylist = new FavoriteMusics(allMusics, user);
            insertPlaylist(favoritePlaylist);
        } catch (IllegalArgumentException e) {
            throw new RuntimeException("Could not create favorite playlist: " + e.getMessage());
        }
    }

    /**
     * Adds a song to the user's favorite playlist.
     *
     * @param email        User email.
     * @param musicName    Name of the song.
     * @param musicManager Music manager.
     */
    public void addFavoriteMusic(String musicName, String email, MusicManager musicManager) {
        Map<String, Playlist> userPlaylists = playlists.get(email.toLowerCase());
        if (userPlaylists == null) {
            throw new RuntimeException("User not found");
        }

        Playlist favoritePlaylist = userPlaylists.get("favorite_musics");
        if (favoritePlaylist == null) {
            throw new RuntimeException("Favorite playlist not found");
        }

        Musica musica = musicManager.getMusica(musicName);
        if (musica == null) {
            throw new RuntimeException("Music not found");
        }

        favoritePlaylist.addMusica(musica);
    }
    /**
     * Remove uma música dos favoritos do Utilizador
     *
     * @param email Email do Utilizador
     * @param musicName Nome da música a ser removida
     * @throws RuntimeException se o Utilizador ou música não existirem
     */
    public void removeFavoriteMusic(String musicName, String email) {
        Map<String, Playlist> userPlaylists = playlists.get(email.toLowerCase());
        if (userPlaylists == null) {
            throw new RuntimeException("Utilizador não encontrado");
        }

        Playlist favoritePlaylist = userPlaylists.get("favorite_musics");
        if (favoritePlaylist == null) {
            throw new RuntimeException("Playlist de favoritos não encontrada");
        }

        favoritePlaylist.removeMusica(musicName);
    }

    /**
     * Obtém a lista de músicas favoritas do Utilizador
     *
     * @param email Email do Utilizador
     * @return Lista de músicas favoritas ou null se não existir
     */
    public List<Musica> getFavoriteMusics(String email) {
        Map<String, Playlist> userPlaylists = playlists.get(email.toLowerCase());
        if (userPlaylists == null) {
            return null;
        }

        Playlist favoritePlaylist = userPlaylists.get("favorite_musics");
        if (favoritePlaylist == null) {
            return null;
        }

        return favoritePlaylist.getMusicas();
    }

    /**
     * Verifica se uma música está nos favoritos do Utilizador
     *
     * @param email Email do Utilizador
     * @param musicName Nome da música
     * @return true se a música estiver nos favoritos, false caso contrário
     */
    public boolean isMusicFavorite(String email, String musicName) {
        List<Musica> favorites = getFavoriteMusics(email);
        if (favorites == null) {
            return false;
        }

        return favorites.stream()
                .anyMatch(m -> m.getNome().equalsIgnoreCase(musicName));
    }

    public void removeMusicFromAllPlaylists(String musicName) {
        for (var userPlaylists : playlists.values()) {
            for (var playlist : userPlaylists.values()) {
                playlist.removeMusica(musicName);
            }
        }
    }

    public void removeAllPlaylistsFromUser(String email){
        this.playlists.remove(email.toLowerCase());
    }

    public void save(String filePath) throws IOException {
        try (ObjectOutputStream out = new ObjectOutputStream(new FileOutputStream(filePath))) {
            out.writeObject(getAllPlaylists());
        }
    }

    @SuppressWarnings("unchecked")
    public void load(String filePath) throws IOException, ClassNotFoundException {
        List<Playlist> playlists;
        try (ObjectInputStream in = new ObjectInputStream(new FileInputStream(filePath))) {
            playlists = (List<Playlist>) in.readObject();
        }
        PlaylistManager manager = new PlaylistManager();
        for (Playlist p : playlists) {
            manager.insertPlaylist(p);
        }
        this.playlists.clear();
        this.playlists.putAll(manager.playlists);
    }
}