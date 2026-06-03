package spotifum.Musics.implementations;

import java.io.Serial;
import java.util.List;
import spotifum.Musics.Musica;

/**
 * Representa uma música explícita, subclass de Musica.
 * Mantém todas as propriedades e comportamentos de Musica,
 * podendo ser usada para distinguir músicas explícitas.
 */
public class Explicita extends Musica {

    @Serial
    private static final long serialVersionUID = 1L;

    /**
     * Construtor vazio que invoca o construtor da superclasse.
     */
    public Explicita(){
        super();
    }

    /**
     * Construtor completo que recebe os dados da música explícita.
     *
     * @param nome Nome da música.
     * @param interprete Nome do intérprete.
     * @param nomeEditora Nome da editora.
     * @param letra Letra da música.
     * @param musica Lista de strings representando a partitura ou linhas da música.
     * @param generoMusical Género musical da música.
     * @param duracaoSegundos Duração da música em segundos.
     */
    public Explicita(String nome, String interprete, String nomeEditora, String letra, List<String> musica, GeneroMusical generoMusical, int duracaoSegundos){
        super(nome, interprete, nomeEditora, letra, musica, generoMusical, duracaoSegundos);
    }

    /**
     * Construtor de cópia para criar uma nova instância a partir de outra existente.
     *
     * @param e Objeto Explicita a copiar.
     */
    public Explicita(Explicita e){
        super(e);
    }

    /**
     * Cria e retorna uma cópia desta instância.
     *
     * @return Uma nova instância Explicita idêntica a esta.
     */
    @Override
    public Explicita clone(){
        return new Explicita(this);
    }

    /**
     * Verifica se outro objeto é igual a esta instância.
     *
     * @param obj Objeto a comparar.
     * @return true se o objeto for da mesma classe e tiver os mesmos valores; false caso contrário.
     */
    @Override
    public boolean equals(Object obj) {
        if (this == obj) return true;
        if (obj == null || this.getClass() != obj.getClass()) return false;

        return super.equals(obj);
    }

    /**
     * Retorna a representação em string desta música explícita.
     *
     * @return String com os detalhes da música.
     */
    @Override
    public String toString(){
        return super.toString();
    }
}