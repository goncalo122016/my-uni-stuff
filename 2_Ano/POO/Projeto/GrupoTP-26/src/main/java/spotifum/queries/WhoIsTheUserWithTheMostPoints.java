package spotifum.queries;

import java.io.Serializable;
import java.util.List;
import spotifum.Users.UserManager;
import spotifum.Users.Utilizador;

/**
 * Consulta que retorna o utilizador com o maior número de pontos.
 */
public class WhoIsTheUserWithTheMostPoints implements Serializable {

    /**
     * Executa a query para encontrar o utilizador com mais pontos.
     *
     * @param managerUser Gerenciador de utilizadores.
     * @return Utilizador com maior pontuação.
     */
    public Utilizador executeQuery(UserManager managerUser) {
        List<Utilizador> users = managerUser.getAllUsers();

        double pontosMax = -1;
        Utilizador userMax = null;

        for (Utilizador user : users) {
            double pontos = user.getPontos();
            if (pontos > pontosMax) {
                pontosMax = pontos;
                userMax = user;
            }
        }
        return userMax;
    }
}
