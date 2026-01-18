package com.restaurante.business.gestEstatisticas;

public class EstatisticasRestaurante extends Estatisticas {

  private double lucro;
  private int pedidos;

  public EstatisticasRestaurante(double lucro, int pedidos) {
    this.lucro = lucro;
    this.pedidos = pedidos;
  }

  public double getLucro() {
    return lucro;
  }

  public int getPedidos() {
    return pedidos;
  }

  @Override
  public String getTipo() {
    return "RESTAURANTE";
  }

  @Override
  public int getTotalPedidos() {
    return pedidos;
  }

  @Override
  public double getTotalReceita() {
    return lucro;
  }
}
