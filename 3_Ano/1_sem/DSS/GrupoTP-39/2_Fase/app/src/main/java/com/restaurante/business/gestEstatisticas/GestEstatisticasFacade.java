package com.restaurante.business.gestEstatisticas;

import com.restaurante.business.gestPedidos.Pedido;
import com.restaurante.data.dao.EstatisticasDAO;
import com.restaurante.data.dao.PedidoDAO;
import com.restaurante.data.dao.PerfilDAO;

import java.util.ArrayList;
import java.util.List;

public class GestEstatisticasFacade implements IGestEstatisticas {
  private final EstatisticasDAO estatisticasDAO;
  private final PedidoDAO pedidoDAO; // para cálculo dinâmico
  private final PerfilDAO perfilDAO;
  private final Autenticacao autenticacao;

  public GestEstatisticasFacade() {
    this.estatisticasDAO = new EstatisticasDAO();
    this.pedidoDAO = new PedidoDAO();
    this.perfilDAO = new PerfilDAO();
    this.autenticacao = new Autenticacao(this.estatisticasDAO);
  }

  public GestEstatisticasFacade(EstatisticasDAO estatisticasDAO, PedidoDAO pedidoDAO, PerfilDAO perfilDAO) {
    this.estatisticasDAO = estatisticasDAO;
    this.pedidoDAO = pedidoDAO;
    this.perfilDAO = perfilDAO;
    this.autenticacao = new Autenticacao(this.estatisticasDAO);
  }

  @Override
  public String login(String username, String password) {
    return autenticacao.autenticar(username, password);
  }

  @Override
  public EstatisticasCadeia calcularEstatisticasCadeia() {
    try {
      List<Pedido> pedidos = pedidoDAO.readAll();
      int total = pedidos.size();
      double receita = 0.0;
      for (Pedido p : pedidos) {
        receita += p.calculaTotal();
      }
      EstatisticasCadeia stats = new EstatisticasCadeia(receita, total);
      estatisticasDAO.create(stats);

      return stats;
    } catch (Exception e) {
      e.printStackTrace();
      return new EstatisticasCadeia(0.0, 0);
    }
  }

  @Override
  public EstatisticasRestaurante calcularEstatisticasRestaurante(String idRestaurante) {
    try {
      List<Pedido> pedidos = pedidoDAO.readByRestaurante(idRestaurante);
      int total = pedidos.size();
      double receita = 0.0;
      for (Pedido p : pedidos) {
        if (p.isPago() == false)
          continue;
        receita += p.calculaTotal();
      }
      EstatisticasRestaurante stats = new EstatisticasRestaurante(receita, total);
      estatisticasDAO.create(stats);

      return stats;
    } catch (Exception e) {
      e.printStackTrace();
      return new EstatisticasRestaurante(0.0, 0);
    }
  }

  /** Lista todos os pedidos de um restaurante específico */
  public List<Pedido> listarPedidosPorRestaurante(String idRestaurante) {
    try {
      return pedidoDAO.readByRestaurante(idRestaurante);
    } catch (Exception e) {
      e.printStackTrace();
      return new ArrayList<>();
    }
  }

  /** Lista todos os restaurantes (id, nome) */
  public List<Restaurante> listarRestaurantes() {
    try {
      return estatisticasDAO.listarRestaurantes();
    } catch (Exception e) {
      e.printStackTrace();
      return new ArrayList<>();
    }
  }

  @Override
  public String getRestauranteIdPorUsername(String username) {
    try {
      return perfilDAO.getRestauranteIdPorUsername(username);
    } catch (Exception e) {
      e.printStackTrace();
      return null;
    }
  }
}
