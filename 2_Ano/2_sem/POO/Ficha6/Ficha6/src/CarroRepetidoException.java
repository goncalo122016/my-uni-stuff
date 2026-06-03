public class CarroRepetidoException extends RuntimeException {
    public CarroRepetidoException(String matricula) {
        super("Carro com matrícula " + matricula + " já existe.");
    }
}
