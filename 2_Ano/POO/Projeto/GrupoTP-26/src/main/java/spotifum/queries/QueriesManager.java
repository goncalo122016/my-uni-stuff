package spotifum.queries;

import java.io.Serializable;
import java.time.LocalDateTime;

import spotifum.Musics.MusicManager;
import spotifum.Musics.Musica;
import spotifum.Musics.implementations.GeneroMusical;
import spotifum.Playlists.PlaylistManager;
import spotifum.Users.UserManager;
import spotifum.Users.Utilizador;

/**
 * Classe que encapsula e centraliza diversas queries
 * para execução sobre os gestores de utilizadores, músicas e playlists.
 * Permite consultar informações agregadas e específicas do sistema.
 */
public class QueriesManager implements Serializable {

    private HowManyPublicPlaylistsExist howManyPublicPlaylistsExist;
    private UserThatListenedToMoreMusics userThatListenedToMoreMusics;
    private WhatGenreOfMusicIsTheMostPlayed whatGenreOfMusicIsTheMostPlayed;
    private WhichIsTheMostReproducedMusic whichIsTheMostReproducedMusic;
    private WhoIsTheUserWithTheMostPoints whoIsTheUserWithTheMostPoints;
    private WhoIsTheMostListenedInterpreter whoIsTheMostListenedInterpreter;
    private WhoIsTheUserWithTheMostPlaylists whoIsTheUserWithTheMostPlaylists;

    /**
     * Construtor padrão que inicializa as queries internas.
     */
    public QueriesManager() {
        this.howManyPublicPlaylistsExist = new HowManyPublicPlaylistsExist();
        this.userThatListenedToMoreMusics = new UserThatListenedToMoreMusics();
        this.whatGenreOfMusicIsTheMostPlayed = new WhatGenreOfMusicIsTheMostPlayed();
        this.whichIsTheMostReproducedMusic = new WhichIsTheMostReproducedMusic();
        this.whoIsTheMostListenedInterpreter = new WhoIsTheMostListenedInterpreter();
        this.whoIsTheUserWithTheMostPlaylists = new WhoIsTheUserWithTheMostPlaylists();
        this.whoIsTheUserWithTheMostPoints = new WhoIsTheUserWithTheMostPoints();
    }

    /**
     * Executa a query para obter a contagem de playlists públicas existentes.
     *
     * @param managerPlaylist Gestor das playlists.
     * @return Número de playlists públicas.
     */
    public int executeQueryHowManyPublicPalylistsExist(PlaylistManager managerPlaylist) {
        return this.howManyPublicPlaylistsExist.executeQuery(managerPlaylist);
    }

    /**
     * Executa a query para obter o utilizador que ouviu mais músicas num intervalo de datas.
     *
     * @param managerUser Gestor dos utilizadores.
     * @param dataInicio Data inicial do intervalo.
     * @param dataFim Data final do intervalo.
     * @return Utilizador que mais ouviu músicas no intervalo.
     * @throws RuntimeException Caso as datas sejam inválidas.
     */
    public Utilizador executeQueryUserThatListenedToMoreMusics(UserManager managerUser, LocalDateTime dataInicio, LocalDateTime dataFim) {
        return this.userThatListenedToMoreMusics.executeQuery(managerUser, dataInicio, dataFim);
    }

    /**
     * Executa a query para obter o utilizador que ouviu mais músicas no geral.
     *
     * @param managerUser Gestor dos utilizadores.
     * @return Utilizador que mais ouviu músicas no geral.
     */
    public Utilizador executeQueryUserThatListenedToMoreMusics(UserManager managerUser) {
        return this.userThatListenedToMoreMusics.executeQuery(managerUser);
    }

    /**
     * Executa a query para obter o género musical mais reproduzido.
     *
     * @param managerMusic Gestor das músicas.
     * @return Género musical mais reproduzido.
     */
    public GeneroMusical executeQueryWhatGenreOfMusicIsTheMostPlayed(MusicManager managerMusic) {
        return this.whatGenreOfMusicIsTheMostPlayed.executeQuery(managerMusic);
    }

    /**
     * Executa a query para obter a música mais reproduzida.
     *
     * @param managerMusic Gestor das músicas.
     * @return Música com o maior número de reproduções.
     */
    public Musica executeQueryWhichIsTheMostReproducedMusic(MusicManager managerMusic) {
        return this.whichIsTheMostReproducedMusic.executeQuery(managerMusic);
    }

    /**
     * Executa a query para obter o intérprete mais ouvido.
     *
     * @param managerMusic Gestor das músicas.
     * @return Nome do intérprete mais reproduzido.
     */
    public String executeQueryWhoIsTheMostListenedInterpreter(MusicManager managerMusic) {
        return this.whoIsTheMostListenedInterpreter.executeQuery(managerMusic);
    }

    /**
     * Executa a query para obter o utilizador com mais playlists criadas.
     *
     * @param managerPlaylist Gestor das playlists.
     * @param managerUser Gestor dos utilizadores.
     * @return Utilizador que possui mais playlists.
     */
    public Utilizador executeQueryWhoIsTheUserWithTheMostPlaylists(PlaylistManager managerPlaylist, UserManager managerUser) {
        return this.whoIsTheUserWithTheMostPlaylists.executeQuery(managerPlaylist, managerUser);
    }

    /**
     * Executa a query para obter o utilizador com mais pontos acumulados.
     *
     * @param managerUser Gestor dos utilizadores.
     * @return Utilizador com maior pontuação.
     */
    public Utilizador executeQueryWhoIsTheUserWithTheMostPoints(UserManager managerUser) {
        return this.whoIsTheUserWithTheMostPoints.executeQuery(managerUser);
    }
}
