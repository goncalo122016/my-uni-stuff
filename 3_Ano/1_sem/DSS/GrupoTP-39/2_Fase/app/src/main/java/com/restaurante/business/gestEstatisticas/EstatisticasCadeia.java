package com.restaurante.business.gestEstatisticas;

public class EstatisticasCadeia extends Estatisticas {

  private double lucroTotal;
  private int totalPedidos;

  public EstatisticasCadeia(double lucroTotal, int totalPedidos) {
    this.lucroTotal = lucroTotal;
    this.totalPedidos = totalPedidos;
  }

  public double getLucroTotal() {
    return lucroTotal;
  }

  public int getTotalPedidosValue() {
    return totalPedidos;
  }

  @Override
  public String getTipo() {
    return "CADEIA";
  }

  @Override
  public int getTotalPedidos() {
    return totalPedidos;
  }

  @Override
  public double getTotalReceita() {
    return lucroTotal;
  }
}
