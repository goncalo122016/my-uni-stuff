public class CarroInexistenteException extends Exception {
    public CarroInexistenteException(String matricula) {
        super("Carro com matrícula " + matricula + " não existe.");
    }
}
