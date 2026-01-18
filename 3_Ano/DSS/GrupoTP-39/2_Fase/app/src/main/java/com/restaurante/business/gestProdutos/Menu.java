package com.restaurante.business.gestProdutos;

public class Menu extends Produto {
  private final Prato prato;
  private final Bebida bebida;

  public Menu(String id, String designacao, Prato prato, Bebida bebida) {
    super(id, designacao, prato.getPreco() + bebida.getPreco(), true);
    this.prato = prato;
    this.bebida = bebida;
  }

  @Override
  public boolean validaDisponibilidade() {
    return prato.validaDisponibilidade() && bebida.validaDisponibilidade();
  }

  public Prato getPrato() {
    return prato;
  }

  public Bebida getBebida() {
    return bebida;
  }

  @Override
  public String toString() {
    return String.format(
        "Menu: %s + %s (%.2f€)", prato.getDesignacao(), bebida.getDesignacao(), getPreco());
  }
}
