package com.restaurante.business.gestProdutos;

import java.util.ArrayList;
import java.util.List;

public class Prato extends Produto {
  private List<Ingrediente> opcoes;
  private List<Ingrediente> composicao;

  public Prato(
      String id,
      String designacao,
      double preco,
      boolean disponivel,
      List<Ingrediente> composicao) {
    super(id, designacao, preco, disponivel);
    this.composicao = composicao;
    this.opcoes = new ArrayList<>();
  }

  @Override
  public boolean validaDisponibilidade() {
    for (Ingrediente ingrediente : composicao) {
      if (!ingrediente.isDisponivel()) {
        return false;
      }
    }
    return true;
  }

  public List<Ingrediente> getComposicao() {
    return composicao;
  }

  public List<Ingrediente> getOpcoes() {
    return opcoes;
  }

  public void setOpcoes(List<Ingrediente> opcoes) {
    this.opcoes = (opcoes != null) ? opcoes : new ArrayList<>();
  }
}
