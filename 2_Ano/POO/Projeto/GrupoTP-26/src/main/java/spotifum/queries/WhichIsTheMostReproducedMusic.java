package spotifum.queries;

import java.io.Serializable;
import java.util.List;
import spotifum.Musics.MusicManager;
import spotifum.Musics.Musica;

/**
 * Consulta que retorna a música com o maior número de reproduções.
 */
public class WhichIsTheMostReproducedMusic implements Serializable {

    /**
     * Executa a query para encontrar a música mais reproduzida.
     *
     * @param managerMusic Gerenciador de músicas.
     * @return Música mais reproduzida.
     */
    public Musica executeQuery(MusicManager managerMusic) {
        List<Musica> musicas = managerMusic.getAllMusics();

        Musica musica = null;
        int reproducoesMax = -1;

        for (Musica music : musicas) {
            int reproducoes = music.getNumeroReproducoes();
            if (reproducoes > reproducoesMax) {
                reproducoesMax = reproducoes;
                musica = music;
            }
        }
        return musica;
    }
}
