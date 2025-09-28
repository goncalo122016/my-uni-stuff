package spotifum.queries;

import java.io.Serializable;
import java.util.List;

import spotifum.Musics.MusicManager;
import spotifum.Musics.Musica;
import spotifum.Musics.implementations.GeneroMusical;

/**
 * Consulta que determina qual o género musical mais reproduzido,
 * somando o número total de reproduções de todas as músicas de cada género.
 */
public class WhatGenreOfMusicIsTheMostPlayed implements Serializable {

    /**
     * Executa a query para identificar o género musical com o maior número de reproduções.
     *
     * @param managerMusic Gerenciador das músicas.
     * @return Género musical mais reproduzido no sistema.
     */
    public GeneroMusical executeQuery(MusicManager managerMusic) {
        int count[] = new int[GeneroMusical.values().length];

        List<Musica> musicas = managerMusic.getAllMusics();

        for (Musica musica : musicas) {
            GeneroMusical genero = musica.getGenero();

            switch (genero) {
                case POP:
                    count[0] += musica.getNumeroReproducoes();
                    break;
                case ROCK:
                    count[1] += musica.getNumeroReproducoes();
                    break;
                case CLASSICA:
                    count[2] += musica.getNumeroReproducoes();
                    break;
                case JAZZ:
                    count[3] += musica.getNumeroReproducoes();
                    break;
                case HIPHOP:
                    count[4] += musica.getNumeroReproducoes();
                    break;
                case ELETRONICA:
                    count[5] += musica.getNumeroReproducoes();
                    break;
                case REGGAE:
                    count[6] += musica.getNumeroReproducoes();
                    break;
                case FADO:
                    count[7] += musica.getNumeroReproducoes();
                    break;
                case OUTRO:
                    count[8] += musica.getNumeroReproducoes();
                    break;
                default:
                    break;
            }
        }

        int indexMax = max(count);

        return GeneroMusical.values()[indexMax];
    }

    /**
     * Método auxiliar que encontra o índice do maior valor no array.
     *
     * @param array Array de inteiros.
     * @return Índice do maior valor encontrado.
     */
    private static int max(int[] array) {
        int maxIndex = 0;

        for (int j = 1; j < array.length; j++) {
            if (array[j] > array[maxIndex]) {
                maxIndex = j;
            }
        }
        return maxIndex;
    }
}
