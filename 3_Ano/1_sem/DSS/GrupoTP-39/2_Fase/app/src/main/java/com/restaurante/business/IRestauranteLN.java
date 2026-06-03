package com.restaurante.business;

import com.restaurante.business.gestEstatisticas.*;
import com.restaurante.business.gestPedidos.*;
import com.restaurante.business.gestProdutos.*;
import com.restaurante.business.gestEntregas.*;
import java.util.List;

public interface IRestauranteLN {

  // GESTÃO DE PRODUTOS
  // =====================================================
  List<Produto> listarProdutos();

  List<Menu> listarMenus(List<Prato> pratos, List<Bebida> bebidas);
  // =====================================================

  // GESTÃO DE PEDIDOS
  // =====================================================
  void registarPedido(Pedido p);

  Pedido consultarPedido(String idPedido);

  boolean processarPagamento(String idPedido, MetodoPagamento metodo);

  double calcularTotalPedido(String idPedido);

  List<Pedido> listarPedidosFila();
  
  boolean moverPedidoNaFila(int fromIndex, int toIndex);

  boolean marcarPedidoPronto(int index);

  boolean removerPedidoDaFila(int index);

  boolean recolherPedido(String idPedido);
  
  List<Pedido> listarTodosPedidos();

  List<Pedido> listarPedidosNaoConfirmados();

  public boolean confirmarPedido(String idPedido, String idRest);
  // =====================================================

  // GESTÃO DE ENTREGAS
  // =====================================================
  List<Entrega> listarEntregas();

  void criarEntregaCustomizada(Pedido p, TipoEntrega tipo, String endereco) throws Exception;

  public String getRestauranteIdPorUsername(String username);

  List<Entrega> listarEntregasPorRestaurante(String idRestaurante);

  void atualizarEntrega(Entrega entrega) throws Exception;
  // =====================================================

  // GESTÃO DE ESTATÍSTICAS
  // =====================================================
  String login(String username, String password);

  Estatisticas getEstatisticas();

  Estatisticas getEstatisticasRestaurante(String idRestaurante);

  List<Restaurante> listarRestaurantes();

  List<Pedido> listarPedidosPorRestaurante(String idRestaurante);
  // =====================================================
}
