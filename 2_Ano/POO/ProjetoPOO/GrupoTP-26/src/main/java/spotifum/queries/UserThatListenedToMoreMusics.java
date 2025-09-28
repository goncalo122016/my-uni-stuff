package spotifum.queries;

import java.io.Serializable;
import java.time.LocalDateTime;
import java.util.List;
import spotifum.Reproductions.ReproducaoMusical;
import spotifum.Users.UserManager;
import spotifum.Users.Utilizador;

/**
 * Consulta que identifica o utilizador que ouviu mais músicas,
 * podendo ser dentro de um intervalo de datas ou considerando todas as reproduções.
 */
public class UserThatListenedToMoreMusics implements Serializable {

    /**
     * Executa a query para encontrar o utilizador com mais reproduções dentro do intervalo de datas.
     *
     * @param managerUser Gerenciador de utilizadores.
     * @param dataInicial Data inicial do intervalo (inclusive).
     * @param dataFinal Data final do intervalo (inclusive).
     * @return Utilizador que ouviu mais músicas no intervalo especificado.
     * @throws RuntimeException Caso as datas sejam inválidas (null ou dataInicial após dataFinal).
     */
    public Utilizador executeQuery(UserManager managerUser, LocalDateTime dataInicial, LocalDateTime dataFinal) throws RuntimeException {
        if (dataInicial == null || dataFinal == null || dataInicial.isAfter(dataFinal)) {
            throw new RuntimeException("Dates entered are invalid");
        }

        List<Utilizador> users = managerUser.getAllUsers();
        Utilizador userMax = null;
        int reproducoes = 0;

        for (Utilizador user : users) {
            int reproducoesUser = 0;
            for (ReproducaoMusical reproducao : user.getReproducaoMusical()) {
                if (reproducao.getDataReproducao().isAfter(dataInicial) && reproducao.getDataReproducao().isBefore(dataFinal)) {
                    reproducoesUser++;
                }
            }
            if (reproducoes < reproducoesUser) {
                reproducoes = reproducoesUser;
                userMax = user;
            }
        }
        return userMax;
    }

    /**
     * Executa a query para encontrar o utilizador que mais reproduções tem no sistema (sem filtro por data).
     *
     * @param managerUser Gerenciador de utilizadores.
     * @return Utilizador com o maior número total de reproduções.
     */
    public Utilizador executeQuery(UserManager managerUser) {
        List<Utilizador> users = managerUser.getAllUsers();
        Utilizador userMax = null;
        int reproducoes = -1;

        for (Utilizador user : users) {
            int reproducoesUser = user.getReproducaoMusical().size();
            if (reproducoes < reproducoesUser) {
                reproducoes = reproducoesUser;
                userMax = user;
            }
        }
        return userMax;
    }
}
