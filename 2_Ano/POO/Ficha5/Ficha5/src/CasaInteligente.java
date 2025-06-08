import java.util.*;

public class CasaInteligente {
    private Map<String, List<Lampada>> divisaoLampadas;
    private List<Lampada> lampadas;

    public void setUpLayout(Collection<String> nomesDivisoes){
        for (String div : nomesDivisoes){
            this.divisaoLampadas.putIfAbsent(div, new ArrayList<>());
        }
    }

    public void addLampada(Lampada l, String divisao){
        if (!this.divisaoLampadas.get(divisao).contains(l)){
            this.divisaoLampadas.get(divisao).add(l);
        }
        else{
            this.divisaoLampadas.putIfAbsent(divisao, new ArrayList<>());
            this.divisaoLampadas.get(divisao).add(l);
        }
    }

    public boolean existeDivisao(String divisao){
        return this.divisaoLampadas.containsKey(divisao);
    }

    public CasaInteligente() {
        this.divisaoLampadas = new HashMap<>();
        this.lampadas = new ArrayList<>();
    }

    public CasaInteligente(Map<String, List<Lampada>> divisaoLampadas, List<Lampada> lampadas) {
        this.divisaoLampadas = new HashMap<>(divisaoLampadas);
        this.lampadas = new ArrayList<>(lampadas);
    }

    public CasaInteligente(CasaInteligente ci) {
        this.divisaoLampadas = new HashMap<>(ci.getDivisaoLampadas());
        this.lampadas = new ArrayList<>(ci.getTodasLampadas());
    }

    public List<Lampada> getTodasLampadas() {
        return new ArrayList<>(this.lampadas);
    }

    public List<Lampada> getLampadasDivisao(String divisao) {
        return new ArrayList<>(this.divisaoLampadas.getOrDefault(divisao, new ArrayList<>()));
    }

    public void adicionaLampada(Lampada l, String divisao) {
        this.lampadas.add(l);
        this.divisaoLampadas.putIfAbsent(divisao, new ArrayList<>());
        this.divisaoLampadas.get(divisao).add(l);
    }

    public void ligaLampadasDivisao(String divisao) {
        List<Lampada> lampadas = this.divisaoLampadas.get(divisao);
        if (lampadas != null) {
            for (Lampada l : lampadas) {
                l.lampON();
            }
        }
    }

    public void desligaLampadasDivisao(String divisao) {
        List<Lampada> lampadas = this.divisaoLampadas.get(divisao);
        if (lampadas != null) {
            for (Lampada l : lampadas) {
                l.lampOFF();
            }
        }
    }

    public void ecoLampadasDivisao(String divisao) {
        List<Lampada> lampadas = this.divisaoLampadas.get(divisao);
        if (lampadas != null) {
            for (Lampada l : lampadas) {
                l.lampECO();
            }
        }
    }

    public double consumoTotalCasa() {
        double total = 0;
        for (Lampada l : this.lampadas) {
            total += l.totalConsumo();
        }
        return total;
    }

    public double consumoTotalDivisao(String divisao) {
        double total = 0;
        List<Lampada> lampadas = this.divisaoLampadas.get(divisao);
        if (lampadas != null) {
            for (Lampada l : lampadas) {
                total += l.totalConsumo();
            }
        }
        return total;
    }

    public CasaInteligente clone() {
        return new CasaInteligente(this);
    }

    @Override
    public String toString() {
        StringBuilder sb = new StringBuilder();
        sb.append("Casa Inteligente: \n");
        for (String divisao : this.divisaoLampadas.keySet()) {
            sb.append("Divisão: ").append(divisao).append("\n");
            sb.append("Lâmpadas: \n");
            for (Lampada l : this.divisaoLampadas.get(divisao)) {
                sb.append(l.toString()).append("\n");
            }
        }
        return sb.toString();
    }

    @Override
    public boolean equals(Object o) {
        if (this == o) return true;
        if (o == null || getClass() != o.getClass()) return false;

        CasaInteligente that = (CasaInteligente) o;

        return this.divisaoLampadas.equals(that.divisaoLampadas) &&
                this.lampadas.equals(that.lampadas);
    }

    public Map<String, List<Lampada>> getDivisaoLampadas() {
        return new HashMap<>(this.divisaoLampadas);
    }
}
