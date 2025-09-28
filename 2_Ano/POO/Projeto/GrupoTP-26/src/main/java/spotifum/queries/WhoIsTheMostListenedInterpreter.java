package spotifum.queries;

import java.io.Serializable;
import java.util.Collections;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import spotifum.Musics.MusicManager;
import spotifum.Musics.Musica;

/**
 * Consulta que retorna o intérprete com o maior total de reproduções em todas as suas músicas.
 */
public class WhoIsTheMostListenedInterpreter implements Serializable {

    /**
     * Executa a query para identificar o intérprete mais ouvido.
     *
     * @param managerMusic Gerenciador de músicas.
     * @return Nome do intérprete mais reproduzido.
     */

    public String executeQuery(MusicManager managerMusic){
        Map<String, Integer> reproducoesInterprete = new HashMap<>();

        List<Musica> musicas = managerMusic.getAllMusics();

        for(Musica m : musicas){
            String interprete = m.getInterprete();
            int reproducoes = m.getNumeroReproducoes();

            if (reproducoesInterprete.containsKey(interprete)){
                int reproducoesAtuais = reproducoesInterprete.get(interprete);
                reproducoesInterprete.put(interprete, reproducoes + reproducoesAtuais);
            }
            else {
                reproducoesInterprete.put(interprete, reproducoes);
            }
        }
        return Collections.max(reproducoesInterprete.entrySet(), Map.Entry.comparingByValue()).getKey();
    }
}
