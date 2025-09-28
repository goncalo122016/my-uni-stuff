package spotifum.Reproductions;

import java.io.Serial;
import java.time.*;
import java.io.Serializable;

/**
 * Representa uma reprodução musical, contendo informações
 * sobre a música reproduzida, o momento da reprodução e o email do utilizador que reproduziu.
 * Implementa {@link Serializable} para permitir a serialização do objeto.
 */
public class ReproducaoMusical implements ReproducaoMusicalInterface, Serializable {
    @Serial
    private static final long serialVersionUID = 1L;

    private String musica;
    private LocalDateTime dataReproducao;
    private String email;

    /**
     * Construtor vazio que inicializa os campos com valores padrão.
     * A data de reprodução será a data e hora atual.
     */
    public ReproducaoMusical() {
        this.musica = "";
        this.dataReproducao = LocalDateTime.now();
        this.email = "";
    }

    /**
     * Construtor parametrizado.
     *
     * @param musica Nome da música reproduzida.
     * @param data Data e hora da reprodução.
     * @param email Email do utilizador que fez a reprodução.
     */
    public ReproducaoMusical(String musica, LocalDateTime data, String email) {
        this.musica = musica;
        this.dataReproducao = data;
        this.email = email;
    }

    /**
     * Construtor de cópia.
     * Cria uma nova instância copiando os dados da reprodução passada.
     *
     * @param rm Objeto ReproducaoMusical a copiar.
     */
    public ReproducaoMusical(ReproducaoMusical rm) {
        this.musica = rm.getMusica();
        this.dataReproducao = rm.getDataReproducao();
        this.email = rm.getEmail();
    }

    /**
     * Retorna o nome da música reproduzida.
     *
     * @return Nome da música.
     */
    public String getMusica() {
        return this.musica;
    }

    /**
     * Retorna a data e hora da reprodução.
     *
     * @return Data e hora da reprodução.
     */
    public LocalDateTime getDataReproducao() {
        return this.dataReproducao;
    }

    /**
     * Retorna o email do utilizador que fez a reprodução.
     *
     * @return Email do utilizador.
     */
    public String getEmail() {
        return this.email;
    }

    /**
     * Define o nome da música reproduzida.
     *
     * @param musica Nome da música.
     */
    public void setMusica(String musica) {
        this.musica = musica;
    }

    /**
     * Define a data e hora da reprodução.
     *
     * @param data Data e hora da reprodução.
     */
    public void setDataReproducao(LocalDateTime data) {
        this.dataReproducao = data;
    }

    /**
     * Define o email do utilizador que reproduziu a música.
     *
     * @param email Email do utilizador.
     */
    public void setEmail(String email) {
        this.email = email;
    }

    /**
     * Cria e retorna uma cópia profunda deste objeto.
     *
     * @return Cópia do objeto ReproducaoMusical.
     */
    @Override
    public ReproducaoMusical clone() {
        return new ReproducaoMusical(this);
    }

    /**
     * Compara este objeto com outro para verificar igualdade.
     * Dois objetos são iguais se possuem a mesma música, data de reprodução e email.
     *
     * @param obj Objeto a comparar.
     * @return true se forem iguais; false caso contrário.
     */
    @Override
    public boolean equals(Object obj) {
        if (this == obj) return true;
        if (obj == null || getClass() != obj.getClass()) return false;
        ReproducaoMusical other = (ReproducaoMusical) obj;
        return musica.equals(other.musica)
                && dataReproducao.equals(other.dataReproducao)
                && email.equals(other.email);
    }

    /**
     * Representação textual da reprodução musical.
     *
     * @return String com os detalhes da reprodução.
     */
    @Override
    public String toString() {
        return String.format("{ musica: \"%s\", data: \"%s\", email: \"%s\" }",
                musica, dataReproducao, email);
    }
}
