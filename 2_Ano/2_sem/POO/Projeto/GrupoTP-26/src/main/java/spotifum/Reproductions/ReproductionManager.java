package spotifum.Reproductions;

import java.io.FileInputStream;
import java.io.FileOutputStream;
import java.io.IOException;
import java.io.ObjectInputStream;
import java.io.ObjectOutputStream;
import java.time.LocalDateTime;
import java.util.ArrayList;
import java.util.List;
import java.util.stream.Collectors;

/**
 * Classe responsável pela gestão das reproduções musicais.
 * Permite criar, adicionar, remover, pesquisar e persistir reproduções.
 */
public class ReproductionManager {
    private final List<ReproducaoMusical> reproducoes;

    /**
     * Construtor que inicializa a lista de reproduções vazia.
     */
    public ReproductionManager() {
        this.reproducoes = new ArrayList<>();
    }

    /**
     * Cria uma nova reprodução musical com os dados fornecidos e adiciona à lista.
     *
     * @param nomeMusica Nome da música reproduzida (não pode ser null).
     * @param emailUser Email do utilizador que reproduziu a música (não pode ser null).
     * @param dataReproducao Data e hora da reprodução (não pode ser null).
     * @throws IllegalArgumentException Se algum dos parâmetros for null.
     */
    public void createReproducao(String nomeMusica, String emailUser, LocalDateTime dataReproducao) {
        if (nomeMusica == null || emailUser == null || dataReproducao == null) {
            throw new IllegalArgumentException("None of the input fields can be null.");
        }
        ReproducaoMusical novaReproducao = new ReproducaoMusical(nomeMusica, dataReproducao, emailUser);
        this.reproducoes.add(novaReproducao);
    }

    /**
     * Adiciona uma reprodução musical já existente à lista.
     *
     * @param novaReproducao Objeto ReproducaoMusical a adicionar (não pode ser null).
     * @throws IllegalArgumentException Se a reprodução for null.
     */
    public void addReproducao(ReproducaoMusical novaReproducao) {
        if (novaReproducao == null) {
            throw new IllegalArgumentException("Reproduction cannot be null.");
        }
        this.reproducoes.add(novaReproducao);
    }

    /**
     * Remove reproduções associadas à música e email especificados.
     *
     * @param nomeMusica Nome da música a remover (não pode ser null).
     * @param emailUser Email do utilizador (não pode ser null).
     * @throws IllegalArgumentException Se algum dos parâmetros for null.
     */
    public void removeReproducao(String nomeMusica, String emailUser) {
        if (nomeMusica == null || emailUser == null) {
            throw new IllegalArgumentException("None of the input fields can be null.");
        }
        reproducoes.removeIf(r -> r.getMusica().equalsIgnoreCase(nomeMusica) && r.getEmail().equals(emailUser));
    }

    /**
     * Obtém uma lista com todas as reproduções existentes.
     *
     * @return Nova lista contendo todas as reproduções (cópia defensiva).
     */
    public List<ReproducaoMusical> getAllReproducoes() {
        return new ArrayList<>(this.reproducoes);
    }

    /**
     * Obtém uma lista de reproduções feitas por um utilizador, dado o seu email.
     *
     * @param email Email do utilizador (não pode ser null).
     * @return Lista de reproduções associadas ao email.
     * @throws IllegalArgumentException Se o email for null.
     */
    public List<ReproducaoMusical> getReproducoesByEmail(String email) {
        if (email == null) {
            throw new IllegalArgumentException("Email cannot be null.");
        }
        return this.reproducoes.stream()
                .filter(r -> r.getEmail().equalsIgnoreCase(email))
                .collect(Collectors.toList());
    }

    /**
     * Obtém uma lista de reproduções feitas de uma música específica.
     *
     * @param nomeMusica Nome da música (não pode ser null).
     * @return Lista de reproduções da música.
     * @throws IllegalArgumentException Se o nome da música for null.
     */
    public List<ReproducaoMusical> getReproducoesByMusica(String nomeMusica) {
        if (nomeMusica == null) {
            throw new IllegalArgumentException("Music name cannot be null.");
        }
        return this.reproducoes.stream()
                .filter(r -> r.getMusica().equalsIgnoreCase(nomeMusica))
                .collect(Collectors.toList());
    }

    /**
     * Salva a lista de reproduções num ficheiro usando serialização.
     *
     * @param filePath Caminho do ficheiro onde salvar os dados.
     * @throws IOException Se ocorrer um erro na escrita do ficheiro.
     */
    public void save(String filePath) throws IOException {
        try (ObjectOutputStream out = new ObjectOutputStream(new FileOutputStream(filePath))) {
            out.writeObject(getAllReproducoes());
        }
    }

    /**
     * Carrega a lista de reproduções de um ficheiro usando desserialização.
     * Substitui as reproduções atuais pelas carregadas.
     *
     * @param filePath Caminho do ficheiro para ler os dados.
     * @throws IOException Se ocorrer um erro na leitura do ficheiro.
     * @throws ClassNotFoundException Se a classe do objeto desserializado não for encontrada.
     */
    @SuppressWarnings("unchecked")
    public void load(String filePath) throws IOException, ClassNotFoundException {
        List<ReproducaoMusical> reproducoes;
        try (ObjectInputStream in = new ObjectInputStream(new FileInputStream(filePath))) {
            reproducoes = (List<ReproducaoMusical>) in.readObject();
        }
        this.reproducoes.clear();
        this.reproducoes.addAll(reproducoes);
    }
}
