import java.io.Serializable;
import java.util.*;
import java.util.stream.Collectors;

public class CarRental implements Serializable {
    private Map<String, Carro> carros;

    static private Map<String, Comparator<Carro>> comparadores;
    static {
        comparadores = new HashMap<>();

        // Exemplos de comparadores registados por nome
        comparadores.put("kms", Comparator.comparingDouble(Carro::getTotalKms));
        comparadores.put("kms-reversed", Comparator.comparingDouble(Carro::getTotalKms).reversed());
        comparadores.put("matricula", Comparator.comparing(Carro::getMatricula));
        comparadores.put("economico", Comparator.comparingDouble(Carro::custoPorKm));
    }

    public boolean existeCarro(String cod){
        return carros.containsKey(cod);
    }

    public int quantos(){
        return carros.size();
    }

    public int quantos(String marca){
        return (int) carros.values().stream().filter( c -> c.getMarca().equals(marca)).count();
    }

    public Carro getCarro(String cod) throws CarroInexistenteException {
        if (!this.carros.containsKey(cod)){
            throw new CarroInexistenteException(cod);
        }

        return carros.get(cod).clone();
    }

    public void adicionaCarro(Carro carro) throws CarroRepetidoException {
        if (this.carros.containsKey(carro.getMatricula())) {
            throw new CarroRepetidoException(carro.getMatricula());
        }

        this.carros.put(carro.getMatricula(), carro.clone());
    }

    public void adiciona(Set<Carro> vs){
        for (Carro carro : vs) {
            try {
                adicionaCarro(carro);
            }
            catch (CarroRepetidoException e) {
                throw e;
            }
        }
    }

    public void registarViagem(String codCarro, int numKms) throws CarroInexistenteException {
        getCarro(codCarro).fazViagem(numKms);
    }

    public void atestarCarro(String codCarro) throws CarroInexistenteException {
        getCarro(codCarro).abastecerCarro();
    }

    public Carro obterCarroMaisEconomico(){
        Carro carroEco = null;
        double custoMin = Integer.MAX_VALUE;
        Collection<Carro> carrosValues = carros.values();

        for (Carro carro : carrosValues) {
            if (carro.custoPorKm() < custoMin) {
                carroEco = carro;
                custoMin = carro.custoPorKm();
            }
        }

        if (carroEco == null) return null;
        return carroEco.clone();
    }

    public double obterMediaCustoPorKm(){
        return this.carros.values().stream().mapToDouble(Carro::custoPorKm).average().orElse(0.0);
    }

    public int custoRealKm(String cod) {
        Carro carro = this.carros.get(cod);
        if (carro == null) {
            throw new IllegalArgumentException("Carro com código " + cod + " não existe.");
        }

        double custo = carro.custoPorKm();
        return (int) Math.round(custo * 1.15);
    }

    public Set<Carro> carrosComAlcance(int kms){
        return carros.values().stream().filter(c -> c.getAutonomia() >= kms).map(Carro::clone).collect(Collectors.toSet());
    }

    public Set<CarroEletrico> comBateriaDe(int nivelMinimo){
        return carros.values().stream().filter(c -> c.getClass().equals(CarroEletrico.class) && ((CarroEletrico) c).getBateriaAtual() >= nivelMinimo).map(CarroEletrico.class::cast).map(CarroEletrico::clone).collect(Collectors.toSet());
    }

    public Carro carroComMaisKms(){
        return this.carros.values().stream().max(Carro::compareByKms).map(Carro::clone).orElse(null);
    }

    public Set<CarroEletrico> ordenaCarrosEletricos(){
        return this.carros.values().stream().filter(c -> c instanceof CarroEletrico).map(c -> (CarroEletrico) c.clone()).collect(Collectors.toCollection(TreeSet::new));
    }

    public Iterator<Carro> ordenarCarros(String criterio){
        Comparator<Carro> comp = comparadores.get(criterio);

        return this.carros.values().stream().sorted(comp).map(Carro::clone).iterator();
    }

    public Map<Double, List<Carro>> carrosPorAutonomia(){
        Map<Double, List<Carro>> map = new HashMap<>();

        for (Carro carro : this.carros.values() ){
            if (!map.containsKey(carro.getAutonomia())){
                map.put(carro.getAutonomia(), new ArrayList<>());
            }
            map.get(carro.getAutonomia()).add(carro.clone());
        }

        return map;
    }

    public List<PontosPorKms> verPontos(){
        return this.carros.values().stream().filter( c -> c instanceof CarroEletrico).map(c -> (PontosPorKms) c).collect(Collectors.toList());
    }

    public List<CarroHibrido> getHibridos() {
        return this.carros.values().stream()
                .filter(c -> c instanceof CarroHibrido)
                .map(c -> (CarroHibrido) c)
                .collect(Collectors.toList());
    }

    // Métodos da Classe:
    public CarRental() {
        this.carros = new HashMap<>();
    }

    public CarRental(Map<String, Carro> carros) {
        this.carros = new HashMap<>(carros);
    }

    // Getter
    public Map<String, Carro> getCarros() {
        return new HashMap<>(this.carros);
    }

    public Comparator<Carro> getComparador(String nome) {
        return comparadores.get(nome);
    }

    // Setter
    public void setCarros(Map<String, Carro> carros) {
        this.carros = new HashMap<>(carros);
    }

    public void adicionarComparador(String nome, Comparator<Carro> comp) {
        comparadores.put(nome, comp);
    }

    public void removerComparador(String nome) {
        comparadores.remove(nome);
    }

    public void removerCarro(Carro carro) {
        this.carros.remove(carro.getMatricula());
    }

    @Override
    public String toString() {
        return "CarRental{" +
                "Carros=" + carros.toString() +
                ", Comparadores=" + comparadores.toString() +
                '}';
    }

    @Override
    public boolean equals(Object o) {
        if (this == o) return true;
        if (o == null || getClass() != o.getClass()) return false;
        CarRental carRental = (CarRental) o;
        return Objects.equals(carros, carRental.carros);
    }
}
