package com.restaurante.business.gestProdutos;

public class Bebida extends Produto {
  private float volumeL;
  private int stock;

  public Bebida(
      String id, String designacao, double preco, boolean disponivel, float volumeL, int stock) {
    super(id, designacao, preco, disponivel);
    this.volumeL = volumeL;
    this.stock = stock;
  }

  @Override
  public boolean validaDisponibilidade() {
    return stock > 0;
  }

  public float getVolumeL() {
    return volumeL;
  }

  public int getStock() {
    return stock;
  }

  public void setStock(int stock) {
    this.stock = stock;
  }
}
