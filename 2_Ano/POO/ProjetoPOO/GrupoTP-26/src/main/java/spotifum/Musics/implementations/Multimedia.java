package spotifum.Musics.implementations;

import java.io.Serial;
import java.util.List;
import spotifum.Musics.Musica;

/**
 * Representa uma música do tipo multimédia, com vídeo associado.
 */
public class Multimedia extends Musica {

    @Serial
    private static final long serialVersionUID = 1L;

    private String video;

    /**
     * Construtor vazio.
     * Inicializa o vídeo com uma string vazia.
     */
    public Multimedia() {
        super();
        this.video = "";
    }

    /**
     * Construtor completo.
     *
     * @param nome Nome da música.
     * @param interprete Intérprete da música.
     * @param nomeEditora Nome da editora.
     * @param letra Letra da música.
     * @param musica Lista de linhas da música.
     * @param generoMusical Género musical.
     * @param duracaoSegundos Duração da música em segundos.
     * @param video URL do vídeo associado.
     */
    public Multimedia(String nome, String interprete, String nomeEditora, String letra, List<String> musica,
                      GeneroMusical generoMusical, int duracaoSegundos, String video) {
        super(nome, interprete, nomeEditora, letra, musica, generoMusical, duracaoSegundos);
        this.video = video;
    }

    /**
     * Construtor de cópia.
     *
     * @param m Objeto Multimedia a copiar.
     */
    public Multimedia(Multimedia m) {
        super(m);
        this.video = m.video;
    }

    /**
     * Obtém a URL do vídeo.
     *
     * @return URL do vídeo.
     */
    public String getVideo() {
        return this.video;
    }

    /**
     * Define a URL do vídeo.
     *
     * @param video URL do vídeo.
     */
    public void setVideo(String video) {
        this.video = video;
    }

    /**
     * Cria uma cópia desta instância.
     *
     * @return Nova instância idêntica a esta.
     */
    @Override
    public Multimedia clone(){
        return new Multimedia(this);
    }

    /**
     * Verifica se outro objeto é igual a esta instância.
     *
     * @param obj Objeto a comparar.
     * @return true se forem iguais, false caso contrário.
     */
    @Override
    public boolean equals(Object obj) {
        if (this == obj) return true;
        if (obj == null || this.getClass() != obj.getClass()) return false;

        Multimedia m = (Multimedia) obj;

        return super.equals(obj) && this.video.equals(m.video);
    }

    /**
     * Retorna a representação em string da música multimédia.
     * Inclui a URL do vídeo.
     *
     * @return String com detalhes da música.
     */
    @Override
    public String toString() {
        return super.toString() + " Vídeo: " + video;
    }

}
