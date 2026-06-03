package com.restaurante.business.gestProdutos;

public abstract class Produto {

  private String id;
  private String designacao;
  private double preco;
  private boolean disponivel;

  public Produto(String id, String designacao, double preco, boolean disponivel) {
    this.id = id;
    this.designacao = designacao;
    this.preco = preco;
    this.disponivel = disponivel;
  }

  public abstract boolean validaDisponibilidade();

  public String getId() {
    return id;
  }

  public String getDesignacao() {
    return designacao;
  }

  public double getPreco() {
    return preco;
  }

  public boolean isDisponivel() {
    return disponivel;
  }

  public void setDesignacao(String designacao) {
    this.designacao = designacao;
  }

  public void setPreco(double preco) {
    this.preco = preco;
  }

  public void setDisponivel(boolean disponivel) {
    this.disponivel = disponivel;
  }
}
