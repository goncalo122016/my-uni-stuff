package com.restaurante.business.gestPedidos;

import java.util.List;

public interface IGestPedidos {

  // Métodos de fila de pedidos
  List<Pedido> listarPedidosFila();

  boolean moverPedidoNaFila(int fromIndex, int toIndex);

  boolean marcarPedidoPronto(int index);

  boolean removerPedidoDaFila(int index);

  boolean removerPedidoDaFilaPorId(String idPedido);

  List<Pedido> listarPedidos(boolean pedidosProntos);

  void registarPedido(Pedido p);

  Pedido consultarPedido(String idPedido);

  boolean adicionarProdutoPedido(String idPedido, String idProduto);

  boolean removerProdutoPedido(String idPedido, String idProduto);

  double calcularTotalPedido(String idPedido);

  boolean processarPagamento(String idPedido, MetodoPagamento metodo);

  boolean confirmarPedido(String idPedido, String idRest);

  List<Pedido> listaPedidosNaoConfirmados();
}
