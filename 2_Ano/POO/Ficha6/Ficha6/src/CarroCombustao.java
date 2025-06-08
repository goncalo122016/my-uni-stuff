public class CarroCombustao extends Carro {
    private double litros_deposito;
    private double litros_atual;
    private double consumoC;
    private double preco_litro;

    private double pontosPorKm;
    private double pontosTotais;

    public CarroCombustao() {
        super();
        this.litros_deposito = 0.0;
        this.consumoC = 0.0;
        this.preco_litro = 0.0;
        this.pontosPorKm = 1.0;
        this.pontosTotais = 0.0;
    }

    public CarroCombustao(String matricula, String marca, int ano, double vmedia, double consumo, double total_kms, double kmsParcial,
                         double litros_deposito, double litros_atual, double consumoC, double preco_litro) {
        super(matricula, marca, ano, vmedia, consumo, total_kms, kmsParcial);
        this.litros_deposito = litros_deposito;
        this.litros_atual = litros_atual;
        this.consumoC = consumoC;
        this.preco_litro = preco_litro;
        this.pontosPorKm = 1.0;
        this.pontosTotais = 0.0;
    }

    public CarroCombustao(CarroCombustao c) {
        super(c);
        this.litros_deposito = c.getLitrosDeposito();
        this.litros_atual = c.getLitrosAtual();
        this.consumoC = c.getConsumoC();
        this.preco_litro = c.getPrecoLitro();
        this.pontosPorKm = c.getPontosPorKm();
        this.pontosTotais = c.getTotalPontos();
    }

    public double custoPorKm(){
        return consumoC/100 * preco_litro;
    }

    public void abastecerCarro(){
        litros_atual = litros_deposito;
        super.setKmsParcial(0);
    }

    public double getLitrosDeposito() {
        return litros_deposito;
    }

    public double getLitrosAtual() {
        return litros_atual;
    }

    public double getConsumoC() {
        return consumoC;
    }

    public double getPrecoLitro() {
        return preco_litro;
    }

    public double getPontosPorKm() {
        return pontosPorKm;
    }

    public double getTotalPontos() {
        return pontosTotais;
    }

    public void setLitrosDeposito(double litros_deposito) {
        this.litros_deposito = litros_deposito;
    }

    public void setLitrosAtual(double litros_atual) {
        this.litros_atual = litros_atual;
    }

    public void setConsumo100(double consumo) {
        this.consumoC = consumo;
    }

    public void setPrecoLitro(double preco_litro) {
        this.preco_litro = preco_litro;
    }

    public void setPontosPorKm(double pontosPorKm) {
        this.pontosPorKm = pontosPorKm;
    }

    public void setPontosTotais(double pontosTotais) {
        this.pontosTotais = pontosTotais;
    }

    @Override
    public CarroCombustao clone() {
        return new CarroCombustao(this);
    }

    @Override
    public boolean equals(Object o) {
        if (this == o) return true;
        if (!(o instanceof CarroCombustao)) return false;
        if (!super.equals(o)) return false;

        CarroCombustao that = (CarroCombustao) o;
        return that.litros_deposito == litros_deposito &&
                that.litros_atual == litros_atual &&
                that.consumoC == consumoC &&
                that.preco_litro == preco_litro &&
                that.pontosPorKm == pontosPorKm &&
                that.pontosTotais == pontosTotais;
    }

    @Override
    public String toString() {
        return "Carro Combustão{" +
                "Carro{" +
                "matricula='" + super.getMatricula() + '\'' +
                ", marca='" + super.getMarca() + '\'' +
                ", ano=" + super.getAno() +
                ", vmedia=" + super.getVmedia() +
                ", consumo=" + super.getAutonomia() +
                ", total_kms=" + super.getTotalKms() +
                ", litros_deposito=" + litros_deposito +
                ", litros_atual=" + litros_atual +
                ", consumo=" + consumoC +
                ", preco_litro=" + preco_litro +
                ", pontosPorKm=" + pontosPorKm +
                ", pontosTotais=" + pontosTotais +
                '}';
    }
}
