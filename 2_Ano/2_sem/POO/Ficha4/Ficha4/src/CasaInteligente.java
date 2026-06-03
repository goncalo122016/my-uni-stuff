import java.util.ArrayList;
import java.util.List;

public class CasaInteligente {
    List<Lampada> lampadas;

    public CasaInteligente(){
        this.lampadas = new ArrayList<>();
    }

    public CasaInteligente(List<Lampada> lampadas){
        this.lampadas = lampadas;
    }

    public List<Lampada> getLampadas(){
        return this.lampadas;
    }

    public void setLampadas(List<Lampada> lampadas){
        this.lampadas = lampadas;
    }

    public void addLampada(Lampada l){
        this.lampadas.add(l.clone());
    }

    public void ligaLampadaNormal(int index){
        this.lampadas.get(index).lampON();
    }

    public void ligaLampadaEco(int index){
        this.lampadas.get(index).lampECO();
    }

    public int qtEmEco(){
        return (int) this.lampadas.stream().filter(l -> l.getModo() == ModoLampada.ECO).count();
    }

    public void removeLampada(int index){
        this.lampadas.remove(index);
    }

    public void ligaTodasEco(){
        this.lampadas.forEach(Lampada::lampECO);
    }

    public void ligaTodasMax(){
        this.lampadas.forEach(Lampada::lampON);
    }

    public double consumoTotal(){
        return this.lampadas.stream().mapToDouble(Lampada::getConsumoTotal).sum();
    }

    public boolean equals(Object o){
        if (this == o) return true;
        if (o == null || getClass() != o.getClass()) return false;

        CasaInteligente c = (CasaInteligente) o;
        return this.lampadas.equals(c.getLampadas());
    }
}
