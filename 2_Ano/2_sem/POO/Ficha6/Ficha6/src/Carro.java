import java.io.Serializable;
import java.util.Comparator;

public abstract class Carro implements PontosPorKms, Serializable {
    private String matricula;
    private String marca;
    private int ano;
    private double vmedia;
    private double autonomia;
    private double total_kms;
    private double kms_parcial;

    public Carro() {
        this.matricula = "";
        this.marca = "";
        this.ano = 0;
        this.vmedia = 0.0;
        this.autonomia = 0.0;
        this.total_kms = 0.0;
        this.kms_parcial = 0.0;
    }

    public Carro(String matricula, String marca, int ano, double vmedia, double autonomia, double total_kms, double kms_parcial) {
        this.matricula = matricula;
        this.marca = marca;
        this.ano = ano;
        this.vmedia = vmedia;
        this.autonomia = autonomia;
        this.total_kms = total_kms;
        this.kms_parcial = kms_parcial;
    }

    public Carro(Carro c) {
        this.matricula = c.getMatricula();
        this.marca = c.getMarca();
        this.ano = c.getAno();
        this.vmedia = c.getVmedia();
        this.autonomia = c.getAutonomia();
        this.total_kms = c.getTotalKms();
        this.kms_parcial = c.getKmsParcial();
    }

    public void fazViagem(int numKms){
        this.total_kms += numKms;
    }

    public abstract double custoPorKm();

    public abstract void abastecerCarro();

    // Getters
    public String getMatricula() {
        return matricula;
    }

    public String getMarca() {
        return marca;
    }

    public int getAno() {
        return ano;
    }

    public double getVmedia() {
        return vmedia;
    }

    public double getAutonomia() {
        return autonomia;
    }

    public double getTotalKms() {
        return total_kms;
    }

    public double getKmsParcial() {
        return kms_parcial;
    }

    // Setters
    public void setMatricula(String matricula) {
        this.matricula = matricula;
    }

    public void setMarca(String marca) {
        this.marca = marca;
    }

    public void setAno(int ano) {
        this.ano = ano;
    }

    public void setVmedia(double vmedia) {
        this.vmedia = vmedia;
    }

    public void setAutonomia(double consumo) {
        this.autonomia = consumo;
    }

    public void setTotalKms(double total_kms) {
        this.total_kms = total_kms;
    }

    public void setKmsParcial(double kms_parcial) {
        this.kms_parcial = kms_parcial;
    }

    public abstract Carro clone();

    @Override
    public boolean equals(Object o) {
        if (this == o) return true;
        if (o == null || getClass() != o.getClass()) return false;

        Carro carro = (Carro) o;
        return ano == carro.ano &&
                vmedia == carro.vmedia &&
                autonomia == carro.autonomia &&
                total_kms == carro.total_kms &&
                matricula.equals(carro.matricula) &&
                marca.equals(carro.marca) &&
                kms_parcial == carro.kms_parcial;
    }

    public abstract String toString();

    public int compareByKms(Carro other) {
        int cmp = Double.compare(other.getTotalKms(), this.getTotalKms());
        if (cmp != 0) return cmp;
        return other.getMatricula().compareTo(this.getMatricula());
    }
}
