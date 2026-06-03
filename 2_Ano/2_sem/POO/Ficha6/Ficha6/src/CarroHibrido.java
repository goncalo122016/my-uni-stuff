public class CarroHibrido extends Carro {
    private double litrosDeposito;
    private double litrosAtual;
    private double consumoCombustao;
    private double precoLitro;

    private double tamanhoBateria;
    private double bateriaAtual;
    private double consumoEletrico;
    private double precoKw;

    private double pontosPorKm;
    private double pontosTotais;

    public CarroHibrido() {
        super();
        this.litrosDeposito = 0.0;
        this.litrosAtual = 0.0;
        this.consumoCombustao = 0.0;
        this.precoLitro = 0.0;
        this.tamanhoBateria = 0.0;
        this.bateriaAtual = 0.0;
        this.consumoEletrico = 0.0;
        this.precoKw = 0.0;
        this.pontosPorKm = 3.0;
        this.pontosTotais = 0.0;
    }

    public CarroHibrido(String matricula, String marca, int ano, double vmedia, double autonomia,
                        double totalKms, double kmsParcial,
                        double litrosDeposito, double litrosAtual, double consumoCombustao, double precoLitro,
                        double tamanhoBateria, double bateriaAtual, double consumoEletrico, double precoKw) {
        super(matricula, marca, ano, vmedia, autonomia, totalKms, kmsParcial);
        this.litrosDeposito = litrosDeposito;
        this.litrosAtual = litrosAtual;
        this.consumoCombustao = consumoCombustao;
        this.precoLitro = precoLitro;
        this.tamanhoBateria = tamanhoBateria;
        this.bateriaAtual = bateriaAtual;
        this.consumoEletrico = consumoEletrico;
        this.precoKw = precoKw;
        this.pontosPorKm = 3.0;
        this.pontosTotais = 0.0;
    }

    public CarroHibrido(CarroHibrido c) {
        super(c);
        this.litrosDeposito = c.litrosDeposito;
        this.litrosAtual = c.litrosAtual;
        this.consumoCombustao = c.consumoCombustao;
        this.precoLitro = c.precoLitro;
        this.tamanhoBateria = c.tamanhoBateria;
        this.bateriaAtual = c.bateriaAtual;
        this.consumoEletrico = c.consumoEletrico;
        this.precoKw = c.precoKw;
        this.pontosPorKm = c.pontosPorKm;
        this.pontosTotais = c.pontosTotais;
    }

    public double custoPorKm() {
        // Combina os dois custos (poderia ponderar pela autonomia restante, aqui é simplificado)
        double custoCombustao = consumoCombustao / 100.0 * precoLitro;
        double custoEletrico = consumoEletrico / 100.0 * precoKw;
        return custoCombustao + custoEletrico;
    }

    public void abastecerCarro() {
        this.litrosAtual = this.litrosDeposito;
        this.bateriaAtual = this.tamanhoBateria;
        super.setKmsParcial(0);
    }

    // Getters

    public double getLitrosDeposito() {
        return this.litrosDeposito;
    }

    public double getLitrosAtual() {
        return this.litrosAtual;
    }

    public double getConsumoCombustao() {
        return this.consumoCombustao;
    }

    public double getPrecoLitro() {
        return this.precoLitro;
    }

    public double getTamanhoBateria() {
        return this.tamanhoBateria;
    }

    public double getBateriaAtual() {
        return this.bateriaAtual;
    }

    public double getConsumoEletrico() {
        return this.consumoEletrico;
    }

    public double getPrecoKw() {
        return this.precoKw;
    }

    // Setters

    public void setLitrosDeposito(double litros) {
        this.litrosDeposito = litros;
    }

    public void setLitrosAtual(double litros) {
        this.litrosAtual = litros;
    }

    public void setConsumoCombustao(double consumo) {
        this.consumoCombustao = consumo;
    }

    public void setPrecoLitro(double preco) {
        this.precoLitro = preco;
    }

    public void setTamanhoBateria(double tamanho) {
        this.tamanhoBateria = tamanho;
    }

    public void setBateriaAtual(double bateria) {
        this.bateriaAtual = bateria;
    }

    public void setConsumoEletrico(double consumo) {
        this.consumoEletrico = consumo;
    }

    public void setPrecoKw(double preco) {
        this.precoKw = preco;
    }

    // PontosPorKms
    public void setPontosPorKm(double pontos) {
        this.pontosPorKm = pontos;
    }

    public double getPontosPorKm() {
        return this.pontosPorKm;
    }

    public double getTotalPontos() {
        return this.pontosTotais;
    }

    public void setPontosTotais(double pontosTotais) {
        this.pontosTotais = pontosTotais;
    }

    @Override
    public Carro clone() {
        return new CarroHibrido(this);
    }

    @Override
    public String toString() {
        return "Carro Híbrido{" +
                "matricula='" + super.getMatricula() + '\'' +
                ", marca='" + super.getMarca() + '\'' +
                ", ano=" + super.getAno() +
                ", vmedia=" + super.getVmedia() +
                ", total_kms=" + super.getTotalKms() +
                ", litrosAtual=" + litrosAtual +
                ", bateriaAtual=" + bateriaAtual +
                ", pontosPorKm=" + pontosPorKm +
                ", pontosTotais=" + pontosTotais +
                '}';
    }

    @Override
    public boolean equals(Object o) {
        if (this == o) return true;
        if (!(o instanceof CarroHibrido)) return false;
        if (!super.equals(o)) return false;
        CarroHibrido that = (CarroHibrido) o;
        return this.litrosDeposito == that.litrosDeposito &&
                this.litrosAtual == that.litrosAtual &&
                this.consumoCombustao == that.consumoCombustao &&
                this.precoLitro == that.precoLitro &&
                this.tamanhoBateria == that.tamanhoBateria &&
                this.bateriaAtual == that.bateriaAtual &&
                this.consumoEletrico == that.consumoEletrico &&
                this.precoKw == that.precoKw &&
                this.pontosPorKm == that.pontosPorKm &&
                this.pontosTotais == that.pontosTotais;
    }
}
