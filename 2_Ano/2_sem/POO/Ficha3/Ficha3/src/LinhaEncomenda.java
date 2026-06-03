public class LinhaEncomenda {
    private String referencia;
    private String descricao;
    private double preco;
    private int qnt;
    private double imp;
    private double desconto;

    public double calculaValorLinhaEnc(){
        double preco = this.qnt * this.preco;
        return preco + preco * this.imp - preco * this.desconto;
    }

    public double calculaValorDesconto(){
        double preco = this.qnt * this.preco;
        return preco * this.desconto;
    }

    public LinhaEncomenda(){
        this.referencia = "";
        this.descricao = "";
        this.preco = 0;
        this.qnt = 0;
        this.imp = 0;
        this.desconto = 0;
    }

    public LinhaEncomenda(String r, String d, double p, int q, double imp, double desconto){
        this.referencia = r;
        this.descricao = d;
        this.preco = p;
        this.qnt = q;
        this.imp = imp;
        this.desconto = desconto;
    }

    public LinhaEncomenda(LinhaEncomenda l){
        this.referencia = l.getRef();
        this.descricao = l.getDescr();
        this.preco = l.getPreco();
        this.qnt = l.getQnt();
        this.imp = l.getImp();
        this.desconto = l.getDesconto();
    }

    public String getRef(){
        return this.referencia;
    }

    public String getDescr(){
        return this.descricao;
    }

    public double getPreco(){
        return this.preco;
    }

    public int getQnt(){
        return this.qnt;
    }

    public double getImp(){
        return this.imp;
    }

    public double getDesconto(){
        return this.desconto;
    }

    public void setRef(String ref){
        this.referencia = ref;
    }

    public void setDescr(String desc){
        this.descricao = desc;
    }

    public void setPreco(double p){
        this.preco = p;
    }

    public void setQnt(int q){
        this.qnt = q;
    }

    public void setImp(double imp){
        this.imp = imp;
    }

    public void setDesconto(double desconto){
        this.desconto = desconto;
    }

    public boolean equals(LinhaEncomenda s){
        if (this == s){
            return true;
        }
        if ((s == null) || (this.getClass() != s.getClass())) {
            return false;
        }
        LinhaEncomenda other = (LinhaEncomenda) s;
        return ((this.referencia.equals(other.getRef())) && (this.descricao.equals(other.getDescr()) && (this.preco == other.getPreco()) && (this.qnt == other.getQnt())) && (this.imp == other.getImp()) && (this.desconto == other.getDesconto()));
    }

    public String toString() {
        StringBuilder sb = new StringBuilder();
        sb.append("Referência: ").append(this.referencia).append("\n");
        sb.append("Descrição: ").append(this.descricao).append("\n");
        sb.append("Preço: ").append(this.preco).append("\n");
        sb.append("Quantidade: ").append(this.qnt).append("\n");
        sb.append("Imposto: ").append(this.imp).append("\n");
        sb.append("Desconto: ").append(this.desconto).append("\n");
        return sb.toString();
    }

    public LinhaEncomenda clone() {
        return new LinhaEncomenda(this);
    }
}
