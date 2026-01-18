package com.restaurante.business.gestPedidos;

import com.restaurante.business.gestProdutos.Produto;
import java.time.LocalDateTime;
import java.util.ArrayList;
import java.util.List;

public class Pedido {

  private String id;
  private boolean pronto;
  private LocalDateTime dataHora;
  private boolean pago;
  private List<ItemPedido> items;
  private double totalArmazenado;

  public Pedido(
      String id, boolean pronto, LocalDateTime dataHora, boolean pago, List<ItemPedido> items) {
    this.id = id;
    this.pronto = pronto;
    this.dataHora = dataHora;
    this.pago = pago;
    this.items = (items != null) ? items : new ArrayList<>();
    this.totalArmazenado = 0.0;
  }

  public Pedido(
      String id,
      boolean pronto,
      LocalDateTime dataHora,
      boolean pago,
      List<ItemPedido> items,
      double total) {
    this.id = id;
    this.pronto = pronto;
    this.dataHora = dataHora;
    this.pago = pago;
    this.items = (items != null) ? items : new ArrayList<>();
    this.totalArmazenado = total;
  }

  public double calculaTotal() {
    if (items != null && !items.isEmpty()) {
      double total = 0.0;
      for (ItemPedido item : items) {
        total += item.calculaTotal();
      }
      if (total > 0) {
        return total;
      }
    }
    return totalArmazenado;
  }

  public void setTotalArmazenado(double total) {
    this.totalArmazenado = total;
  }

  public boolean confirmaPagamento(MetodoPagamento metodo) {
    if (metodo == null || this.pago) {
      return false;
    }
    this.pago = true;
    return true;
  }

  public List<ItemPedido> getItems() {
    return (items != null) ? items : new ArrayList<>();
  }

  public int getQuantidade(int idProduto) {
    return getQuantidade(String.valueOf(idProduto));
  }

  public int getQuantidade(String idProduto) {
    if (items == null || idProduto == null) {
      return 0;
    }
    for (ItemPedido item : items) {
      if (item.getProduto() != null && item.getProduto().getId().equals(idProduto)) {
        return item.getQuantidade();
      }
    }
    return 0;
  }

  public boolean addItem(Produto produto, int quantidade) {
    if (items == null || produto == null || quantidade <= 0) {
      return false;
    }
    for (ItemPedido item : items) {
      if (item.getProduto() != null && item.getProduto().getId().equals(produto.getId())) {
        item.setQuantidade(item.getQuantidade() + quantidade);
        return true;
      }
    }
    ItemPedido novoItem = new ItemPedido(quantidade, "", produto, new ArrayList<>());
    items.add(novoItem);
    return true;
  }

  public boolean removeItem(String idProduto) {
    if (items == null || idProduto == null) {
      return false;
    }
    return items.removeIf(
        item -> item.getProduto() != null && item.getProduto().getId().equals(idProduto));
  }

  public String getId() {
    return id;
  }

  public boolean isPronto() {
    return pronto;
  }

  public LocalDateTime getDataHora() {
    return dataHora;
  }

  public boolean isPago() {
    return pago;
  }

  public void setPronto(boolean pronto) {
    this.pronto = pronto;
  }

  public void setPago(boolean pago) {
    this.pago = pago;
  }
}
