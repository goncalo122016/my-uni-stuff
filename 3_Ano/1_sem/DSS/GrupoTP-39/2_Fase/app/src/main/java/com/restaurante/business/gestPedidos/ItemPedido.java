package com.restaurante.business.gestPedidos;

import com.restaurante.business.gestProdutos.Ingrediente;
import com.restaurante.business.gestProdutos.Produto;
import java.util.List;

public class ItemPedido {
  /** Calcula o preço total deste item (produto + extras) */
  public double calculaTotal() {
    double total = 0.0;
    if (produto != null) {
      total += produto.getPreco();
    }
    if (extras != null && !extras.isEmpty()) {
      for (Ingrediente ing : extras) {
        total += ing.getPreco();
      }
    }
    return total * quantidade;
  }

  private int quantidade;
  private String notas;
  private Produto produto;
  private List<Ingrediente> extras;

  public ItemPedido(int quantidade, String notas, Produto produto, List<Ingrediente> extras) {
    this.quantidade = quantidade;
    this.notas = notas;
    this.produto = produto;
    this.extras = extras;
  }

  public int getQuantidade() {
    return quantidade;
  }

  public String getNotas() {
    return notas;
  }

  public Produto getProduto() {
    return produto;
  }

  public List<Ingrediente> getExtras() {
    return extras;
  }

  public void setQuantidade(int quantidade) {
    this.quantidade = quantidade;
  }

  public void setNotas(String notas) {
    this.notas = notas;
  }
}
