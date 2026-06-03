import java.time.LocalDateTime;
import java.util.ArrayList;
import java.util.Iterator;
import java.util.List;

public class Encomenda {
    private String cliente;
    private int nif;
    private String morada;
    private int nrEncomenda;
    private LocalDateTime data;
    private List<LinhaEncomenda> linhaEncomenda;

    public double calculaValorTotal(){
        double valorTotal = 0;
        Iterator<LinhaEncomenda> it = linhaEncomenda.iterator();

        while (it.hasNext()) {
            LinhaEncomenda l = it.next();
            valorTotal += l.calculaValorLinhaEnc();
        }
        return valorTotal;
    }

    public double calculaValorDesconto(){
        double valorDesconto = 0;
        Iterator<LinhaEncomenda> it = linhaEncomenda.iterator();

        while (it.hasNext()) {
            LinhaEncomenda l = it.next();
            valorDesconto += l.calculaValorDesconto();
        }
        return valorDesconto;
    }

    public int numeroTotalProdutos(){
        int total = 0;
        Iterator<LinhaEncomenda> it = linhaEncomenda.iterator();
        while (it.hasNext()) {
            LinhaEncomenda l = it.next();
            total += l.getQnt();
        }
        return total;
    }

    public boolean existeProdutoEncomenda(String refProduto){
        Iterator<LinhaEncomenda> it = linhaEncomenda.iterator();
        while (it.hasNext()) {
            LinhaEncomenda l = it.next();
            if (l.getRef().equals(refProduto)){
                return true;
            }
        }
        return false;
    }

    public void adicionaLinha(LinhaEncomenda linha) {
        this.linhaEncomenda.add(linha.clone());
    }

    public void removeProduto(String codProd){
        Iterator<LinhaEncomenda> it = linhaEncomenda.iterator();
        while (it.hasNext()) {
            LinhaEncomenda l = it.next();
            if (l.getRef().equals(codProd)){
                it.remove();
            }
        }
    }

    public Encomenda() {
        this.cliente = "";
        this.nif = 0;
        this.morada = "";
        this.nrEncomenda = 0;
        this.data = LocalDateTime.now();
        this.linhaEncomenda = new ArrayList<>();
    }

    public Encomenda(String cliente, int nif, String morada, int nrEncomenda, LocalDateTime data, LinhaEncomenda[] linhaEncomenda) {
        this.cliente = cliente;
        this.nif = nif;
        this.morada = morada;
        this.nrEncomenda = nrEncomenda;
        this.data = data;
        List<LinhaEncomenda> novo = new ArrayList<>();
        for (LinhaEncomenda a : linhaEncomenda) {
            novo.add(a.clone());
        }
        this.linhaEncomenda = novo;
    }

    public Encomenda(Encomenda e) {
        this.cliente = e.getCliente();
        this.nif = e.getNif();
        this.morada = e.getMorada();
        this.nrEncomenda = e.getNrEncomenda();
        this.data = e.getData();
        this.linhaEncomenda = e.getLinhaEncomenda();
    }

    public String getCliente() {
        return this.cliente;
    }

    public void setCliente(String cliente) {
        this.cliente = cliente;
    }

    public int getNif() {
        return this.nif;
    }

    public void setNif(int nif) {
        this.nif = nif;
    }

    public String getMorada() {
        return this.morada;
    }

    public void setMorada(String morada) {
        this.morada = morada;
    }

    public int getNrEncomenda() {
        return this.nrEncomenda;
    }

    public void setNrEncomenda(int nrEncomenda) {
        this.nrEncomenda = nrEncomenda;
    }

    public LocalDateTime getData() {
        return this.data;
    }

    public void setData(LocalDateTime data) {
        this.data = data;
    }

    // Iterador Externo
    public List<LinhaEncomenda> getLinhaEncomenda() {
        List<LinhaEncomenda> cpy = new ArrayList<>();
        for (LinhaEncomenda linha : this.linhaEncomenda) {
            cpy.add(linha.clone());
        }
        return cpy;
    }

    // Iterador Interno
    public List<LinhaEncomenda> getLinhaEncomendaInt() {
        Iterator<LinhaEncomenda> it = this.linhaEncomenda.iterator();
        List<LinhaEncomenda> cpy = new ArrayList<>();

        while (it.hasNext()){
            LinhaEncomenda l = it.next();
            cpy.add(l.clone());
        }
        return cpy;
    }

    public void setLinhaEncomenda(List<LinhaEncomenda> linhaEncomenda) {
        List<LinhaEncomenda> l = new ArrayList<>();
        for (LinhaEncomenda linha : linhaEncomenda) {
            l.add(linha.clone());
        }
        this.linhaEncomenda = l;
    }

    public boolean equals(Encomenda s){
        if (this == s){
            return true;
        }
        if ((s == null) || (this.getClass() != s.getClass())) {
            return false;
        }
        Encomenda other = (Encomenda) s;
        return ((this.nrEncomenda == other.getNrEncomenda()));
    }

    public String toString() {
        StringBuilder sb = new StringBuilder();
        sb.append("Cliente: " + this.cliente + "\n");
        sb.append("Nif: " + this.nif + "\n");
        sb.append("Morada: " + this.morada + "\n");
        sb.append("NrEncomenda: " + this.nrEncomenda + "\n");
        sb.append("Data: " + this.data + "\n\n");
        Iterator<LinhaEncomenda> it = this.linhaEncomenda.iterator();
        while (it.hasNext()) {
            sb.append(it.next().toString());
            sb.append("\n");
        }
        return sb.toString();
    }

    public Encomenda clone() {
        return new Encomenda(this);
    }
}
