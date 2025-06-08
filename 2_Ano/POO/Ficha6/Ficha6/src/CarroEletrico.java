public class CarroEletrico extends Carro implements Comparable<CarroEletrico> {
    private double tamanho_bateria;
    private double bateria_atual;
    private double consumoE;
    private double preco_kw;

    private double pontosPorKm;
    private double pontosTotais;

    public CarroEletrico() {
        super();
        this.tamanho_bateria = 0.0;
        this.bateria_atual = 0.0;
        this.consumoE = 0.0;
        this.preco_kw = 0.0;
        this.pontosPorKm = 2.5;
        this.pontosTotais = 0.0;
    }

    public CarroEletrico(String matricula, String marca, int ano, double vmedia, double consumo, double total_kms, double kmsParcial,
                         double tamanho_bateria, double bateria_atual, double consumoE, double preco_kw) {
        super(matricula, marca, ano, vmedia, consumo, total_kms, kmsParcial);
        this.tamanho_bateria = tamanho_bateria;
        this.bateria_atual = bateria_atual;
        this.consumoE = consumoE;
        this.preco_kw = preco_kw;
        this.pontosPorKm = 2.5;
        this.pontosTotais = 0.0;
    }

    public CarroEletrico(CarroEletrico c) {
        super(c);
        this.tamanho_bateria = c.getTamanhoBateria();
        this.bateria_atual = c.getBateriaAtual();
        this.consumoE = c.getConsumoE();
        this.preco_kw = c.getPrecoKw();
        this.pontosPorKm = c.getPontosPorKm();
        this.pontosPorKm = c.getPontosPorKm();
        this.pontosTotais = c.getTotalPontos();
    }

    public double custoPorKm(){
        return consumoE/100 * preco_kw;
    }

    public void abastecerCarro(){
        bateria_atual = tamanho_bateria;
        super.setKmsParcial(0);
    }

    // Getters
    public double getTamanhoBateria() {
        return tamanho_bateria;
    }

    public double getBateriaAtual(){
        return bateria_atual;
    }

    public double getConsumoE() {
        return consumoE;
    }

    public double getPrecoKw() {
        return preco_kw;
    }

    public double getPontosPorKm() {
        return pontosPorKm;
    }

    public double getTotalPontos() {
        return pontosTotais;
    }

    // Setters
    public void setTamanhoBateria(double tamanho_bateria) {
        this.tamanho_bateria = tamanho_bateria;
    }

    public void setBateriaAtual(double bateria_atual) {
        this.bateria_atual = bateria_atual;
    }

    public void setConsumoE(double consumoE) {
        this.consumoE = consumoE;
    }

    public void setPrecoKw(double preco_kw) {
        this.preco_kw = preco_kw;
    }

    public void setPontosPorKm(double pontosPorKm) {
        this.pontosPorKm = pontosPorKm;
    }

    public void setPontosTotais(double pontosTotais) {
        this.pontosTotais = pontosTotais;
    }

    @Override
    public CarroEletrico clone() {
        return new CarroEletrico(this);
    }

    @Override
    public boolean equals(Object o) {
        if (this == o) return true;
        if (!(o instanceof CarroEletrico)) return false;
        if (!super.equals(o)) return false;

        CarroEletrico that = (CarroEletrico) o;
        return that.tamanho_bateria == tamanho_bateria &&
                bateria_atual == that.bateria_atual &&
                that.consumoE == consumoE &&
                that.preco_kw == preco_kw &&
                that.pontosPorKm == pontosPorKm &&
                that.pontosTotais == pontosTotais;
    }

    @Override
    public String toString() {
        return "Carro Elétrico{" +
                "matricula='" + super.getMatricula() + '\'' +
                ", marca='" + super.getMarca() + '\'' +
                ", ano=" + super.getAno() +
                ", velocidade média=" + super.getVmedia() +
                ", consumo=" + super.getAutonomia() +
                ", total_kms=" + super.getTotalKms() +
                ", tamanho_bateria=" + tamanho_bateria +
                ", bateria_atual=" + bateria_atual +
                ", consumoE=" + consumoE +
                ", preco_kw=" + preco_kw +
                ", pontosPorKm=" + pontosPorKm +
                ", pontosTotais=" + pontosTotais +
                '}';
    }

    @Override
    public int compareTo(CarroEletrico outro) {
        int cmp = Double.compare(this.getTotalKms(), outro.getTotalKms()); // crescente
        if (cmp != 0) return cmp;
        return Double.compare(outro.getTamanhoBateria(), this.getTamanhoBateria()); // decrescente
    }
}
