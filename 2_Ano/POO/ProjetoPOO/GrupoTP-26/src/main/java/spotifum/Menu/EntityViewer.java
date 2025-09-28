package spotifum.Menu;

import java.util.List;
import java.util.Scanner;

import spotifum.Musics.Musica;
import spotifum.Playlists.Playlist;
import spotifum.Reproductions.ReproducaoMusical;
import spotifum.Spotifum;
import spotifum.Users.Utilizador;

/**
 * Class responsible for viewing entities in the system.
 */
public class EntityViewer {
    private final Spotifum spotifum;
    private InputHandler inputHandler = new InputHandler(new Scanner(System.in));

    public EntityViewer(Spotifum spotifum) {
        this.spotifum = spotifum;
    }

    /**
     * Consults a user by email and prints its information.
     */
    public void consultarUtilizador() {
        String email = inputHandler.readLine("Email: ");
        try {
            Utilizador u = spotifum.getUser(email);
            System.out.println("Utilizador encontrado:\n" + u.toString());
        } catch (RuntimeException e) {
            System.out.println("O Utilizador não existe!");
        }
    }

    /**
     * Prints all users in the system.
     */
    public void consultarTodosUtilizadores() {
        List<Utilizador> users = spotifum.getAllUsers();
        if (users.isEmpty()) {
            System.out.println("Não existem utilizadores no sistema!");
            return;
        }
        System.out.println("Todos os Utilizadores:");
        for (Utilizador u : users) {
            System.out.println(u.toString());
        }
    }

    /**
     * Consults a music by name and prints its information.
     */
    public void consultarMusica() {
        String nome = inputHandler.readLine("Nome: ");
        try {
            Musica m = spotifum.getMusica(nome);
            System.out.println("Música encontrada:\n" + m.toString());
        } catch (RuntimeException e) {
            System.out.println("A Música não existe!");
        }
    }

    /**
     * Prints all musics in the system.
     */
    public void consultarTodasMusicas() {
        List<Musica> musicas = spotifum.getAllMusics();
        if (musicas.isEmpty()) {
            System.out.println("Não existem músicas no sistema!");
            return;
        }
        System.out.println("Todas as Músicas:");
        for (Musica musica : musicas) {
            System.out.println(musica.toString());
        }
    }

    /**
     * Consults a playlist by user's email and playlist name and prints its information.
     */
    public void consultarPlaylist() {
        String email = inputHandler.readLine("Email do Utilizador: ");
        String nome = inputHandler.readLine("Nome da Playlist: ");

        try {
            Playlist p = spotifum.getPlaylist(email, nome);
            System.out.println("Playlist encontrada:\n" + p.toString());
        } catch (RuntimeException e) {
            System.out.println("A Playlist não existe!");
        }
    }

    /**
     * Prints all playlists in the system.
     */
    public void consultarTodasPlaylist() {
        List<Playlist> playlists = spotifum.getAllPlaylists();
        if (playlists.isEmpty()) {
            System.out.println("Não existem playlists no sistema!");
            return;
        }
        System.out.println("Todas as Playlists:");
        for (Playlist playlist : playlists) {
            System.out.println(playlist.toString());
        }
    }

    /**
     * Consults all reproduction records of a user by email and prints them.
     */
    public void consultarReproducoesUtilizador() {
        String email = inputHandler.readLine("Email: ");
        if (!spotifum.existsUser(email)) {
            System.out.println("O Utilizador não existe!");
            return;
        }
        
        List<ReproducaoMusical> historico = spotifum.getReproducoesByEmail(email);
        if (historico.isEmpty()) {
            System.out.println("O Utilizador não tem reproduções!");
            return;
        }
        System.out.println("Todas as Reproduções do Utilizador:");
        for (ReproducaoMusical r : historico) {
            System.out.println(r.toString());
        }
    }
}
