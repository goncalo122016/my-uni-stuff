/*********************************************************************************/
/** DISCLAIMER: Este código foi criado e alterado durante as aulas práticas      */
/** de POO. Representa uma solução em construção, com base na matéria leccionada */ 
/** até ao momento da sua elaboração, e resulta da discussão e experimentação    */
/** durante as aulas. Como tal, não deverá ser visto como uma solução canónica,  */
/** ou mesmo acabada. É disponibilizado para auxiliar o processo de estudo.      */
/** Os alunos são encorajados a testar adequadamente o código fornecido e a      */
/** procurar soluções alternativas, à medida que forem adquirindo mais           */
/** conhecimentos de POO.                                                        */
/*********************************************************************************/

import java.time.LocalDate;
import java.util.*;
import java.util.stream.Collectors;

/**
 * Classe que implementa uma Encomenda (cf exercício da Ficha 4).
 *
 * @author MaterialPOO
 * @version 20180323
 * @version 20190209
 * @version 20220324
 * @version 20230320
 * @version 20240318
 * @version 20250324
 */

public class Encomenda {
    private static int nEnc = 0;
    
    private String nomeCliente;
    private int nif;
    private String morada;
    private LocalDate data;
    private Map<String, LinhaEncomenda> linhas;
    
    /* Métodos de classe */
    
    public static int getNEnc() {
      return nEnc;
    }
    
    public static void incNEnc() {
      nEnc++;
    }
    
    /* Construtores */
    
    public Encomenda(String nomeCliente, int nif, String morada,
                    LocalDate data, Map<String, LinhaEncomenda> linhasEnc) {
        this.nomeCliente = nomeCliente;
        this.nif = nif;
        this.morada = morada;
        this.nEnc = ++nEnc; // podia utilizar os métodos de classe
        /*
         * incNEnc();
         * this.nEnc = getNEnc();
         */
        this.data = data;
        setLinhas(linhasEnc);
    }
    
    public Encomenda() {
        this.linhas = new HashMap<>();
        this.nomeCliente = "n/a";
        this.nif = 0;
        this.morada = "n/a";
        this.nEnc = ++nEnc;
        this.data = LocalDate.now();
    }
    
    public Encomenda(Encomenda enc) {
        this.linhas = enc.getLinhas();
        this.nomeCliente = enc.getNomeCliente();
        this.nif = enc.getNif();
        this.morada = enc.getMorada();
        this.nEnc = enc.getNEnc();
        this.data = enc.getData();
    }
    
    public String getNomeCliente() {
        return this.nomeCliente;
    }
    
    public int getNif() {
        return this.nif;
    }
    
    public String getMorada() {
        return this.morada;
    }
      
    
    public LocalDate getData() {
        return this.data;
    }
    
    public void setLinhas(Map<String, LinhaEncomenda> linhasEnc) {
        this.linhas = new HashMap<>();
        this.linhas.putAll(linhasEnc);
    }
    
    //
    // Uma versão com iteradores internos:
    // 
    // public void setLinhas(List<LinhaEncomenda> linhasEnc) {
    //   this.linhas = linhasEnc.stream().map(LinhaEncomenda::clone)
    //                                   .collect(Collectors.toList());
    // }     
    // 
    
    
    public Map<String, LinhaEncomenda> getLinhas() {
        return this.linhas.entrySet().stream().collect(Collectors.toMap(Map.Entry::getKey, e -> e.getValue()));
    }
    
    /*
    public ArrayList<LinhaEncomenda> getLinhas_1() {
      return this.linhas.stream().map(LinhaEncomenda::clone)
                                 .collect(Collectors.toCollection(ArrayList::new));  
    }
    */
    
    
    /*
     * Versão com iterators
     */

    /*
    public List<LinhaEncomenda> getLinhasIT() {
      List<LinhaEncomenda> res = new ArrayList<>();
      Iterator<LinhaEncomenda> it = this.linhas.iterator();
      
      while (it.hasNext()) {
        LinhaEncomenda le = it.next();  
        res.add(le.clone());
      }
        
      return res;
    }
    */
    
    //public List<LinhaEncomenda> getLinhas() {
    //    List<LinhaEncomenda> res = new ArrayList<>();
    //    for(LinhaEncomenda le : this.linhas) {
    //        res.add(le.clone());
    //    }
    //    return res;
    //}
    
    public Encomenda clone() {
        return new Encomenda(this);
    }
    
    public boolean equals(Object o) {
        if(o==this) return true;
        if(o==null || o.getClass() != this.getClass()) return false;
        Encomenda e = (Encomenda)o;
        return nomeCliente.equals(e.getNomeCliente()) && nif == e.getNif() &&
               morada.equals(e.getMorada()) && nEnc == e.getNEnc() && 
               data.equals(e.getData()) && this.linhas.equals(e.getLinhas()); 
    }
    
    public String toString() {
        StringBuilder sb = new StringBuilder();
        sb.append("Encomenda : [");
        sb.append("Número Encomenda: ").append(this.nEnc);
        sb.append("NomeCliente: ").append(this.nomeCliente);
        sb.append("NIF: ").append(this.nif);
        sb.append("Morada: ").append(this.morada);
        sb.append("Data Enc: ").append(this.data);
        sb.append(this.linhas.toString());
        return sb.toString();
    }
    
    
    /**
     * B)
     */
    public double calculaValorTotal() {
        return this.linhas.values().stream().mapToDouble(LinhaEncomenda::calculaValorLinhaEnc).sum();
    }
    
    /**
     * C)
     */
    
    public double calculaValorDesconto() {
      return this.linhas.values().stream().mapToDouble(LinhaEncomenda::calculaValorDesconto)
                                 .sum();   
    }
    
    /**
     * D)
     */
    public int numeroTotalProdutos() {
        return this.linhas.values().stream().mapToInt(LinhaEncomenda::getQuantidade).sum();
    }

    /**
     * E)
     */
    public boolean existeProdutoEncomenda(String codProd) {
      return linhas.containsKey(codProd);
    }
    
    /**
     * F)
     */
    public void adicionaLinha(LinhaEncomenda linha) {
        this.linhas.put(linha.getReferencia(), linha.clone());
    }

    /**
     * G)
     */
    
    public void removeProduto(String codProd){
        this.linhas.remove(codProd);
    }

    //calcula o número de produtos encomendados nesta encomenda
    public int numProdutos() {
        return this.linhas.size();
    }    
    
    //calcula a lista dos códigos de produto de uma encomenda
    public List<String> getListaProdutos() {
      return this.linhas.keySet().stream().toList();
    }
    
    // a devolver conjunto
    public Set<String> getListaProdutosAsSet() {
       return this.linhas.keySet();
    }
    
    /**
     * Método que devolve um conjunto de linhas de encomenda ordenado pela ordem natural das 
     * linhas de encomenda (que é a ordem crescente da referência de produto)
     */
    
    public Set<LinhaEncomenda> linhasEncomendaPorOrdem() {
        return this.linhas.values().stream()
                .map(LinhaEncomenda::clone)
                .collect(Collectors
                .toCollection(TreeSet::new));
    }
}
