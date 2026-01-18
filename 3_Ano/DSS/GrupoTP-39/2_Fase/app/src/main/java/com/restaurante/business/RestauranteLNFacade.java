package com.restaurante.business;

import com.restaurante.business.gestEstatisticas.*;
import com.restaurante.business.gestPedidos.*;
import com.restaurante.business.gestProdutos.*;
import com.restaurante.business.gestEntregas.*;
import com.restaurante.data.DatabaseManager;

import java.util.ArrayList;
import java.util.List;

public class RestauranteLNFacade implements IRestauranteLN {

  private final IGestProdutos gestProdutos;
  private final GestPedidosFacade gestPedidos;
  private final GestEstatisticasFacade gestEstatisticas;
  private final GestEntregasFacade gestEntregas;

  public RestauranteLNFacade() {
    DatabaseManager.getInstance().initializeDatabase();
    this.gestProdutos = new GestProdutosFacade();
    this.gestPedidos = new GestPedidosFacade();
    this.gestEstatisticas = new GestEstatisticasFacade();
    this.gestEntregas = new GestEntregasFacade();
  }

  // GESTÃO DE PRODUTOS
  @Override
  public List<Produto> listarProdutos() {
    return new ArrayList<>(gestProdutos.listarProdutosDisponiveis());
  }

  @Override
  public List<Menu> listarMenus(List<Prato> pratos, List<Bebida> bebidas) {
    return gestProdutos.listarMenus(pratos, bebidas);
  }

  // GESTÃO DE PEDIDOS
  @Override
  public void registarPedido(Pedido p) {
    if (p == null)
      return;
    gestPedidos.registarPedido(p);
  }

  @Override
  public List<Pedido> listarPedidosNaoConfirmados() {
    return gestPedidos.listaPedidosNaoConfirmados();
  }

  @Override
  public Estatisticas getEstatisticasRestaurante(String idRestaurante) {
    return gestEstatisticas.calcularEstatisticasRestaurante(idRestaurante);
  }

  @Override
  public Pedido consultarPedido(String idPedido) {
    return gestPedidos.consultarPedido(idPedido);
  }

  @Override
  public boolean processarPagamento(String idPedido, MetodoPagamento metodo) {
    return gestPedidos.processarPagamento(idPedido, metodo);
  }

  @Override
  public double calcularTotalPedido(String idPedido) {
    Pedido p = consultarPedido(idPedido);
    if (p == null)
      return 0.0;
    return p.calculaTotal();
  }

  @Override
  public List<Pedido> listarPedidosFila() {
    return gestPedidos.listarPedidosFila();
  }

  @Override
  public boolean moverPedidoNaFila(int fromIndex, int toIndex) {
    return gestPedidos.moverPedidoNaFila(fromIndex, toIndex);
  }

  @Override
  public boolean marcarPedidoPronto(int index) {
    return gestPedidos.marcarPedidoPronto(index);
  }

  @Override
  public boolean removerPedidoDaFila(int index) {
    return gestPedidos.removerPedidoDaFila(index);
  }

  @Override
  public boolean recolherPedido(String idPedido) {
    Pedido p = gestPedidos.consultarPedido(idPedido);
    if (p == null)
      return false;
    if (!p.isPago() || !p.isPronto())
      return false;

    boolean entregaOk = gestEntregas.recolherEntregaPorPedido(idPedido);
    if (!entregaOk)
      return false;

    gestPedidos.removerPedidoDaFilaPorId(idPedido);
    return true;
  }

  @Override
  public List<Pedido> listarTodosPedidos() {
    return gestPedidos.listarTodosPedidos();
  }

  @Override
  public boolean confirmarPedido(String idPedido, String userName) {
    return gestPedidos.confirmarPedido(idPedido, userName);
  }

  @Override
  public String getRestauranteIdPorUsername(String username) {
    return gestEstatisticas.getRestauranteIdPorUsername(username);
  }

  @Override
  public List<Entrega> listarEntregas() {
    return gestEntregas.listarEntregas();
  }

  @Override
  public List<Entrega> listarEntregasPorRestaurante(String idRestaurante) {
    return gestEntregas.listarEntregasPorRestaurante(idRestaurante);
  }

  @Override
  public void criarEntregaCustomizada(Pedido pedido, TipoEntrega tipo, String endereco)
      throws Exception {
    gestEntregas.criarEntregaCustomizada(pedido, tipo, endereco);
  }

  @Override
  public void atualizarEntrega(Entrega entrega) throws Exception {
    gestEntregas.atualizarEntrega(entrega);
  }

  // GESTÃO DE ESTATÍSTICAS
  @Override
  public String login(String username, String password) {
    return gestEstatisticas.login(username, password);
  }

  @Override
  public Estatisticas getEstatisticas() {
    return gestEstatisticas.calcularEstatisticasCadeia();
  }

  @Override
  public List<Restaurante> listarRestaurantes() {
    return gestEstatisticas.listarRestaurantes();
  }

  @Override
  public List<Pedido> listarPedidosPorRestaurante(String idRestaurante) {
    return gestEstatisticas.listarPedidosPorRestaurante(idRestaurante);
  }
}
