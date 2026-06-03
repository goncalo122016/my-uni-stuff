package com.restaurante.business.gestPedidos;

import com.restaurante.business.gestProdutos.Ingrediente;
import com.restaurante.business.gestProdutos.Prato;
import com.restaurante.business.gestProdutos.Produto;
import com.restaurante.data.dao.PedidoDAO;
import com.restaurante.data.dao.ProdutoDAO;
import java.util.LinkedList;
import java.util.List;

public class GestPedidosFacade implements IGestPedidos {
  // Fila de pedidos em memória
  private final LinkedList<Pedido> filaPedidos = new LinkedList<>();

  // Métodos de fila de pedidos
  @Override
  public List<Pedido> listarPedidosFila() {
    return List.copyOf(filaPedidos);
  }

  @Override
  public boolean moverPedidoNaFila(int fromIndex, int toIndex) {
    if (fromIndex < 0 || fromIndex >= filaPedidos.size())
      return false;
    if (toIndex < 0 || toIndex >= filaPedidos.size())
      return false;
    if (fromIndex == toIndex)
      return true;
    Pedido p = filaPedidos.remove(fromIndex);
    filaPedidos.add(toIndex, p);
    return true;
  }

  @Override
  public boolean marcarPedidoPronto(int index) {
    if (index < 0 || index >= filaPedidos.size())
      return false;
    Pedido p = filaPedidos.get(index);
    p.setPronto(true);
    try {
      pedidoDAO.update(p);
    } catch (Exception e) {
      e.printStackTrace();
      return false;
    }
    return true;
  }

  @Override
  public boolean removerPedidoDaFila(int index) {
    if (index < 0 || index >= filaPedidos.size())
      return false;
    return filaPedidos.remove(index) != null;
  }

  @Override
  public boolean removerPedidoDaFilaPorId(String idPedido) {
    if (idPedido == null)
      return false;

    for (int i = 0; i < filaPedidos.size(); i++) {
      if (filaPedidos.get(i).getId().equals(idPedido)) {
        filaPedidos.remove(i);
        return true;
      }
    }
    return false;
  }

  private final PedidoDAO pedidoDAO;
  private final ProdutoDAO produtoDAO;

  public GestPedidosFacade() {
    this.pedidoDAO = new PedidoDAO();
    this.produtoDAO = new ProdutoDAO();
  }

  public GestPedidosFacade(PedidoDAO pedidoDAO) {
    this.pedidoDAO = pedidoDAO;
    this.produtoDAO = new ProdutoDAO();
  }

  public GestPedidosFacade(PedidoDAO pedidoDAO, ProdutoDAO produtoDAO) {
    this.pedidoDAO = pedidoDAO;
    this.produtoDAO = produtoDAO;
  }

  @Override
  public List<Pedido> listarPedidos(boolean pedidosProntos) {
    try {
      if (pedidosProntos) {
        return pedidoDAO.readPedidosProntos();
      } else {
        return pedidoDAO.readAll();
      }
    } catch (Exception e) {
      e.printStackTrace();
      return List.of();
    }
  }

  @Override
  public List<Pedido> listaPedidosNaoConfirmados() {
    try {
      return pedidoDAO.readPedidosNaoConfirmados();
    } catch (Exception e) {
      e.printStackTrace();
      return List.of();
    }
  }

  @Override
  public void registarPedido(Pedido p) {
    if (p == null)
      return;
    // Simula preparação: decrementa ingredientes usados
    if (p.getItems() != null) {
      for (ItemPedido item : p.getItems()) {
        Produto prod = item.getProduto();
        if (prod instanceof Prato prato) {
          for (Ingrediente ing : prato.getComposicao()) {
            ing.decrementarStock(item.getQuantidade());
          }
        }
      }
    }
    try {
      pedidoDAO.create(p);
    } catch (Exception e) {
      e.printStackTrace();
    }
  }

  @Override
  public Pedido consultarPedido(String idPedido) {
    try {
      return pedidoDAO.read(idPedido);
    } catch (Exception e) {
      e.printStackTrace();
      return null;
    }
  }

  @Override
  public boolean adicionarProdutoPedido(String idPedido, String idProduto) {
    try {
      Pedido pedido = pedidoDAO.read(idPedido);
      if (pedido == null) {
        return false;
      }
      Produto produto = produtoDAO.read(idProduto);
      if (produto == null || !produto.isDisponivel()) {
        return false;
      }
      boolean adicionado = pedido.addItem(produto, 1);
      if (adicionado) {
        pedidoDAO.update(pedido);
      }
      return adicionado;
    } catch (Exception e) {
      e.printStackTrace();
      return false;
    }
  }

  @Override
  public boolean removerProdutoPedido(String idPedido, String idProduto) {
    try {
      Pedido pedido = pedidoDAO.read(idPedido);
      if (pedido == null) {
        return false;
      }
      boolean removido = pedido.removeItem(idProduto);
      if (removido) {
        pedidoDAO.update(pedido);
      }
      return removido;
    } catch (Exception e) {
      e.printStackTrace();
      return false;
    }
  }

  @Override
  public double calcularTotalPedido(String idPedido) {
    Pedido p = consultarPedido(idPedido);
    if (p == null)
      return 0.0;
    return p.calculaTotal();
  }

  @Override
  public boolean processarPagamento(String idPedido, MetodoPagamento metodo) {
    Pedido p = consultarPedido(idPedido);
    if (p == null)
      return false;
    boolean ok = p.confirmaPagamento(metodo);
    if (ok) {
      try {
        pedidoDAO.update(p);
      } catch (Exception e) {
        /* ignore */ }
    }
    return ok;
  }

  @Override
  public boolean confirmarPedido(String idPedido, String idRest) {
    try {
      Pedido pedido = pedidoDAO.read(idPedido);
      if (pedido == null) {
        return false;
      }
      if (!pedido.isPago()) {
        System.out.println("⚠ Pedido ainda não foi pago pelo cliente.");
        return false;
      }

      boolean confirmado = pedidoDAO.confirmarPedido(idPedido, idRest);

      if (confirmado) {
        Pedido pedidoAtualizado = consultarPedido(idPedido);

        boolean jaNaFila = filaPedidos.stream().anyMatch(fp -> fp.getId().equals(idPedido));
        if (!jaNaFila && pedidoAtualizado != null) {
          filaPedidos.addLast(pedidoAtualizado);
        }

        return true;
      }

      return false;

    } catch (Exception e) {
      e.printStackTrace();
      return false;
    }
  }

  public List<Pedido> listarTodosPedidos() {
    try {
      return pedidoDAO.readAll();
    } catch (Exception e) {
      e.printStackTrace();
      return List.of();
    }
  }
}
