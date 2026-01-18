package com.restaurante.business.gestEntregas;

import com.restaurante.business.gestPedidos.Pedido;
import com.restaurante.data.dao.EntregaDAO;
import com.restaurante.data.dao.PedidoDAO;
import java.time.LocalDateTime;
import java.util.List;

public class GestEntregasFacade implements IGestEntregas {
  private final EntregaDAO entregaDAO;
  private final PedidoDAO pedidoDAO; // para validar existência de pedido

  public GestEntregasFacade() {
    this.entregaDAO = new EntregaDAO();
    this.pedidoDAO = new PedidoDAO();
  }

  public GestEntregasFacade(EntregaDAO entregaDAO, PedidoDAO pedidoDAO) {
    this.entregaDAO = entregaDAO;
    this.pedidoDAO = pedidoDAO;
  }

  // Permite atualizar uma entrega (ex: estado)
  public void atualizarEntrega(Entrega entrega) throws Exception {
    entregaDAO.update(entrega);
  }

  // Permite criar entrega customizada (tipo e endereço)
  public void criarEntregaCustomizada(Pedido pedido, TipoEntrega tipo, String endereco)
      throws Exception {
    if (pedido == null) return;
    String idEntrega = "ENT-" + System.currentTimeMillis();
    Entrega entrega =
        new Entrega(idEntrega, java.time.LocalDateTime.now(), pedido, tipo, EstadoEntrega.PENDENTE);
    if (endereco != null) entrega.setEndereco(endereco);
    entregaDAO.create(entrega);
  }

  @Override
  public boolean confirmarEntrega(String idEntrega) {
    try {
      Entrega e = entregaDAO.read(idEntrega);
      if (e == null) return false;
      e.setEstado(EstadoEntrega.ENTREGUE);
      e.setDataEntrega(LocalDateTime.now());
      entregaDAO.update(e);
      return true;
    } catch (Exception ex) {
      return false;
    }
  }

  @Override
  public void registarEntrega(Pedido p) {
    if (p == null) return;
    try {
      Pedido existente = pedidoDAO.read(p.getId());
      if (existente == null) {
        pedidoDAO.create(p);
      }
      String idEntrega = "ENT-" + System.currentTimeMillis();
      Entrega entrega =
          new Entrega(
              idEntrega, LocalDateTime.now(), p, TipoEntrega.BALCAO, EstadoEntrega.PENDENTE);
      entregaDAO.create(entrega);
    } catch (Exception e) {
      e.printStackTrace();
    }
  }

  @Override
  public List<Entrega> listarEntregas() {
    try {
      return entregaDAO.readAll();
    } catch (Exception e) {
      e.printStackTrace();
      return List.of();
    }
  }

  @Override
  public List<Entrega> listarEntregasPorRestaurante(String idRestaurante) {
    try {
      return entregaDAO.readEntregasPorRestaurante(idRestaurante);
    } catch (Exception e) {
      e.printStackTrace();
      return List.of();
    }
  }

  @Override
  public List<Entrega> listarEntregasPendentes() {
    try {
      return entregaDAO.readEntregasPendentes();
    } catch (Exception e) {
      e.printStackTrace();
      return List.of();
    }
  }

  @Override
  public Entrega consultarEntrega(String idEntrega) {
    try {
      return entregaDAO.read(idEntrega);
    } catch (Exception e) {
      e.printStackTrace();
      return null;
    }
  }

  @Override
  public boolean recolherEntregaPorPedido(String idPedido) {
    try {
      List<Entrega> entregas = entregaDAO.readAll();
      Entrega entrega =
          entregas.stream()
              .filter(e -> e.getPedido() != null && idPedido.equals(e.getPedido().getId()))
              .findFirst()
              .orElse(null);

      if (entrega == null) return false;

      if (entrega.getEstado() == EstadoEntrega.ENTREGUE) return false;

      entrega.setEstado(EstadoEntrega.ENTREGUE);
      entrega.setDataEntrega(LocalDateTime.now());
      entregaDAO.update(entrega);
      return true;
    } catch (Exception e) {
      return false;
    }
  }
}
