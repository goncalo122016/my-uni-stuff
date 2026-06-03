package com.restaurante.business.gestProdutos;

public class Ingrediente {

  private String id;
  private int stock;
  private String designacao;
  private double preco;

  @Override
  public boolean equals(Object o) {
    if (this == o) return true;
    if (o == null || getClass() != o.getClass()) return false;
    Ingrediente that = (Ingrediente) o;
    return id != null && id.equals(that.id);
  }

  @Override
  public int hashCode() {
    return id != null ? id.hashCode() : 0;
  }

  public Ingrediente(String id, int stock, String designacao, double preco) {
    this.id = id;
    this.stock = stock;
    this.designacao = designacao;
    this.preco = preco;
  }

  public Ingrediente(int stock, String designacao, double preco) {
    this(null, stock, designacao, preco);
  }

  public String getId() {
    return id;
  }

  public void setId(String id) {
    this.id = id;
  }

  public boolean isDisponivel() {
    return stock > 0;
  }

  public String getDesignacao() {
    return designacao;
  }

  public double getPreco() {
    return preco;
  }

  public int getStock() {
    return stock;
  }

  public void decrementarStock(int quantidade) {
    this.stock = Math.max(0, this.stock - quantidade);
  }

  public void incrementarStock(int quantidade) {
    this.stock += quantidade;
  }
}
