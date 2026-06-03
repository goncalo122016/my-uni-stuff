package spotifum;

import java.io.File;
import java.io.IOException;
import java.util.List;
import spotifum.Musics.Musica;
import spotifum.Musics.implementations.GeneroMusical;
import spotifum.Musics.implementations.TipoMusica;
import spotifum.Users.PlanoSubscricao;
import spotifum.Users.UserManager;
import spotifum.Users.Utilizador;
import spotifum.queries.QueriesManager;
import spotifum.Musics.MusicManager;
import spotifum.Playlists.PlayManager;
import spotifum.Playlists.Playlist;
import spotifum.Playlists.PlaylistManager;
import spotifum.Reproductions.ReproducaoMusical;
import spotifum.Reproductions.ReproductionManager;

import java.io.Serializable;
import java.time.LocalDateTime;

public class Spotifum implements Serializable {
    private final UserManager userManager;
    private final MusicManager musicManager;
    private final PlaylistManager playlistManager;
    private final ReproductionManager reproductionManager;
    private final QueriesManager queriesManager;

    private static final String DEFAULT_STATE_DIR = "src/main/java/spotifum/Load_States/";

    public Spotifum() {
        this.userManager = new UserManager();
        this.musicManager = new MusicManager();
        this.playlistManager = new PlaylistManager();
        this.reproductionManager = new ReproductionManager();
        this.queriesManager = new QueriesManager();
    }

    public void loadEstado(String dir) {
        String userFile = DEFAULT_STATE_DIR + dir + "users.ser";
        String musicFile = DEFAULT_STATE_DIR + dir + "musics.ser";
        String playlistFile = DEFAULT_STATE_DIR + dir + "playlists.ser";
        String reproductionFile = DEFAULT_STATE_DIR + dir + "reproductions.ser";

        try {
            userManager.load(userFile);
            musicManager.load(musicFile);
            playlistManager.load(playlistFile);
            reproductionManager.load(reproductionFile);
        } catch (IOException | ClassNotFoundException e) {
            System.out.println("Erro ao carregar estado: " + e.getMessage());
        }
    }

    public void guardarEstado(String dir) {
        File folder = new File(DEFAULT_STATE_DIR + dir);
        if (!folder.exists()) {
            if (!folder.mkdirs()) {
                System.out.println("Erro ao criar diretório: " + dir);
                return;
            }
        }

        String userFile = DEFAULT_STATE_DIR + dir + "users.ser";
        String musicFile = DEFAULT_STATE_DIR + dir + "musics.ser";
        String playlistFile = DEFAULT_STATE_DIR + dir + "playlists.ser";
        String reproductionFile = DEFAULT_STATE_DIR + dir + "reproductions.ser";

        try {
            userManager.save(userFile);
            musicManager.save(musicFile);
            playlistManager.save(playlistFile);
            reproductionManager.save(reproductionFile);
        } catch (IOException e) {
            System.out.println("Erro ao guardar estado: " + e.getMessage());
        }
    }

    public void createUser(String nome, String email, String morada, PlanoSubscricao plano) {
        Utilizador user = this.userManager.createUser(nome, email, morada, plano);
        this.userManager.insertUser(user);
        // pontos, musicasFavoritas e reproducoes são ignorados na criação - pode ajustar se quiser usar
    }

    public void removeUser(String email) {
        this.userManager.removeUser(email);
    }

    public boolean existsUser(String email) {
        return this.userManager.existsUser(email);
    }

    public Utilizador getUser(String email) {
        return this.userManager.getUser(email);
    }

    public void updateUserName(String name, String email) {
        Utilizador user = getUser(email);
        user.setNome(name);
        this.userManager.updateUser(user);
    }

    public void updateUserAddress(String address, String email) {
        Utilizador user = getUser(email);
        user.setMorada(address);
        this.userManager.updateUser(user);
    }

    public void updateUserPlan(PlanoSubscricao plan, String email) {
        Utilizador user = getUser(email);
        user.setPlano(plan);
        this.userManager.updateUser(user);
    }

    public List<Utilizador> getAllUsers() {
        return this.userManager.getAllUsers();
    }

    public void addFavoriteMusic(String nomeMusica, String email) {
        this.userManager.addFavoriteMusic(this.musicManager, nomeMusica, email);
    }

    public void removeFavoriteMusic(String nomeMusica, String email) {
        this.userManager.removeFavoriteMusic(this.musicManager, nomeMusica, email);
    }

    public void addMusicReproduction(String nomeMusica, String email, LocalDateTime data) {
        this.userManager.addMusicReproduction(this.musicManager, nomeMusica, email, data);
    }

    public void createMusica(String nome, String interprete, String nomeEditora, String letra, List<String> musica,
            GeneroMusical genero, int duraçaoSegundos, String video, TipoMusica type) {
        Musica music = this.musicManager.createMusica(nome, interprete, nomeEditora, letra, musica, genero,
                duraçaoSegundos, video, type);
        this.musicManager.insertMusica(music);
    }

    public void removeMusic(String nomeMusica) {
        this.musicManager.removeMusic(nomeMusica);
    }

    public boolean existsMusic(String nomeMusica) {
        return this.musicManager.existsMusic(nomeMusica);
    }

    public Musica getMusica(String nomeMusica) {
        return this.musicManager.getMusica(nomeMusica);
    }

    public List<Musica> getAllMusics() {
        return this.musicManager.getAllMusics();
    }

    public boolean isMusicFavorite(String email, String musicName){
        return this.playlistManager.isMusicFavorite(email, musicName);
    }

    public void createPlaylist(String email, List<String> musicas, String nome, boolean publica) {
        Playlist playlist = this.playlistManager.createPlaylist(this.userManager, this.musicManager, email, musicas,
                nome, publica);
        this.playlistManager.insertPlaylist(playlist);
    }

    public void updateGeneratedMusicsPlaylist(Utilizador user, List<Musica> userMusics, int duration, boolean explicit){
        this.playlistManager.updateGeneratedMusicsPlaylist(user, userMusics, duration, explicit);
    }

    public Playlist getPlaylist(String email, String nome) {
        return this.playlistManager.getPlaylist(email, nome);
    }

    public void removePlaylist(String email, String nome) {
        this.playlistManager.removePlaylist(this.userManager, email, nome);
    }

    public void addFavoriteMusicPlaylist(String musicName, String email){
        this.playlistManager.addFavoriteMusic(musicName, email, this.musicManager);
    }

    public void removeFavoriteMusicPlaylist(String musicName, String email){
        this.playlistManager.removeFavoriteMusic(musicName, email);
    }

    public void removeAllPlaylistsFromUser(String email){
        this.playlistManager.removeAllPlaylistsFromUser(email);
    }

    public boolean existsPlaylist(String email, String nome) {
        return this.playlistManager.existsPlaylist(email, nome);
    }

    public List<Playlist> getAllPlaylists() {
        return this.playlistManager.getAllPlaylists();
    }

    public void removeMusicFromAllPlaylists(String musicName){
        this.playlistManager.removeMusicFromAllPlaylists(musicName);
    }

    public void incrementNReproducoes(String email, String nome){
        this.playlistManager.incrementNReproducoes(email, nome);
    }

    public void createFavoriteMusicsPlaylist(Utilizador u){
        this.playlistManager.createFavoriteMusicsPlaylist(u, this.musicManager);
    }

    public void createGeneratedMusicsPlaylist(Utilizador user, List<Musica> userMusics, int duration, boolean explicit){
        this.playlistManager.createGeneratedMusicsPlaylist(user, userMusics, duration, explicit);
    }

    public PlayManager createPlayManager(){
        return new PlayManager(this.reproductionManager, this.playlistManager, this.musicManager, this.userManager);
    }

    public void createReproducao(String nomeMusica, String emailUser, LocalDateTime dataReproducao){
        this.reproductionManager.createReproducao(nomeMusica, emailUser, dataReproducao);
    }

    public List<ReproducaoMusical> getReproducoesByEmail(String email){
        return this.reproductionManager.getReproducoesByEmail(email);
    }

    public int executeQueryHowManyPublicPalylistsExist() {
        return this.queriesManager.executeQueryHowManyPublicPalylistsExist(this.playlistManager);
    }

    public Utilizador executeQueryUserThatListenedToMoreMusics(LocalDateTime dataInicio, LocalDateTime dataFim) {
        return this.queriesManager.executeQueryUserThatListenedToMoreMusics(this.userManager, dataInicio, dataFim);
    }

    public Utilizador executeQueryUserThatListenedToMoreMusics() {
        return this.queriesManager.executeQueryUserThatListenedToMoreMusics(this.userManager);
    }

    public GeneroMusical executeQueryWhatGenreOfMusicIsTheMostPlayed() {
        return this.queriesManager.executeQueryWhatGenreOfMusicIsTheMostPlayed(this.musicManager);
    }

    public Musica executeQueryWhichIsTheMostReproducedMusic() {
        return this.queriesManager.executeQueryWhichIsTheMostReproducedMusic(this.musicManager);
    }

    public String executeQueryWhoIsTheMostListenedInterpreter() {
        return this.queriesManager.executeQueryWhoIsTheMostListenedInterpreter(this.musicManager);
    }

    public Utilizador executeQueryWhoIsTheUserWithTheMostPlaylists() {
        return this.queriesManager.executeQueryWhoIsTheUserWithTheMostPlaylists(this.playlistManager, this.userManager);
    }

    public Utilizador executeQueryWhoIsTheUserWithTheMostPoints() {
        return this.queriesManager.executeQueryWhoIsTheUserWithTheMostPoints(this.userManager);
    }
}
