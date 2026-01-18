package com.restaurante.ui;

import com.restaurante.business.IRestauranteLN;
import com.restaurante.business.gestPedidos.*;
import com.restaurante.business.gestEntregas.*;
import com.restaurante.business.gestProdutos.*;
import com.restaurante.utils.UtilsMenu;
import java.util.List;
import java.util.Scanner;

public class MenuFuncionario {
  private final Scanner in;
  private final IRestauranteLN ln;
  private String userName;

  public MenuFuncionario(Scanner in, IRestauranteLN ln) {
    this.in = in;
    this.ln = ln;
  }

  public void run() {
    System.out.print("Utilizador: ");
    String user = in.nextLine().trim();
    System.out.print("Password: ");
    String pass = in.nextLine().trim(); 

    if (autentica(user, pass) == null || !autentica(user, pass).equals("FUNCIONARIO")) {
      UtilsMenu.erro("❌ Credenciais inválidas.");
      return;
    }

    userName = user;
    System.out.println("✅ Login efetuado com sucesso. Bem-vindo, " + userName + "!");

    while (true) {
      System.out.println("\n--- Funcionário ---");
      System.out.println("1) Ver fila de pedidos");
      System.out.println("2) Mover pedido");
      System.out.println("3) Marcar pronto");
      System.out.println("4) Remover pedido");
      System.out.println("5) Ver ingredientes");
      System.out.println("6) Confirmar Pedido");
      System.out.println("0) Voltar");
      System.out.print("> ");

      String opt = in.nextLine().trim();
      switch (opt) {
        case "1" -> listarFila();
        case "2" -> moverPedido();
        case "3" -> marcarPronto();
        case "4" -> removerPedido();
        case "5" -> listarIngredientesRestaurante();
        case "6" -> confirmarPedido();
        case "0" -> {
          return;
        }
        default -> System.out.println("Opção inválida.");
      }
    }
  }

  // Lista ingredientes e quantidades do restaurante
  private void listarIngredientesRestaurante() {
    System.out.println("\n--- Ingredientes do Restaurante ---");
    List<Produto> produtos = ln.listarProdutos();
    java.util.Set<String> mostrados = new java.util.HashSet<>();
    for (Produto p : produtos) {
      if (p instanceof Prato prato) {
        for (Ingrediente ing : prato.getComposicao()) {
          if (!mostrados.contains(ing.getDesignacao())) {
            System.out.printf("- %s: %d unidades\n", ing.getDesignacao(), ing.getStock());
            mostrados.add(ing.getDesignacao());
          }
        }
      }
    }
  }

  private void listarFila() {
    List<Pedido> filaPedidos = ln.listarPedidosFila();
    if (filaPedidos == null || filaPedidos.isEmpty()) {
      System.out.println("Fila vazia.");
      return;
    }
    System.out.println("\nFila de pedidos:");
    for (int i = 0; i < filaPedidos.size(); i++) {
      Pedido p = filaPedidos.get(i);
      System.out.printf(
          "%d) %s | pronto=%s | pago=%s | %s\n",
          i + 1, p.getId(), p.isPronto(), p.isPago(), p.getDataHora());
    }
  }

  private void moverPedido() {
    List<Pedido> filaPedidos = ln.listarPedidosFila();
    if (filaPedidos == null || filaPedidos.isEmpty()) {
      System.out.println("Fila vazia.");
      return;
    }
    listarFila();
    System.out.print("Índice do pedido a mover: ");
    int idx = UtilsMenu.readIntSafe(in, 1) - 1;
    if (idx < 0 || idx >= filaPedidos.size()) {
      System.out.println("Índice inválido.");
      return;
    }
    System.out.print("Novo índice (1.." + filaPedidos.size() + "): ");
    int novo = UtilsMenu.readIntSafe(in, 1) - 1;
    if (novo < 0 || novo >= filaPedidos.size()) {
      System.out.println("Índice inválido.");
      return;
    }
    boolean ok = ln.moverPedidoNaFila(idx, novo);
    System.out.println(ok ? "Pedido reposicionado." : "Falha a reposicionar.");
  }

  private void marcarPronto() {
    List<Pedido> filaPedidos = ln.listarPedidosFila();
    if (filaPedidos == null || filaPedidos.isEmpty()) {
      System.out.println("Fila vazia.");
      return;
    }
    listarFila();
    System.out.print("Índice do pedido a executar e marcar como pronto: ");
    int idx = UtilsMenu.readIntSafe(in, 1) - 1;
    if (idx < 0 || idx >= filaPedidos.size()) {
      System.out.println("Índice inválido.");
      return;
    }

    Pedido pedidoParaExecutar = filaPedidos.get(idx);
    System.out.println("\n--- Execução do pedido antes de marcar como pronto ---");
    if (pedidoParaExecutar.getItems() == null) {
      System.out.println("[DEBUG] pedido.getItems() == null");
    } else if (pedidoParaExecutar.getItems().isEmpty()) {
      System.out.println("[DEBUG] pedido.getItems() is empty");
    } else {
      System.out.println(
          "[DEBUG] pedido.getItems().size() = " + pedidoParaExecutar.getItems().size());
      System.out.println("Itens:");
      for (var item : pedidoParaExecutar.getItems()) {
        Produto prod = item.getProduto();
        System.out.println(
            "- "
                + prod.getDesignacao()
                + " x"
                + item.getQuantidade()
                + " [Produto class: "
                + prod.getClass().getName()
                + ", id: "
                + prod.getId()
                + "]");
      }
      System.out.println("\n--- Simulação de preparação ---");
      for (var item : pedidoParaExecutar.getItems()) {
        Produto prod = item.getProduto();
        System.out.println(
            "[DEBUG] Produto: "
                + prod.getDesignacao()
                + " | class: "
                + prod.getClass().getName()
                + " | id: "
                + prod.getId());
        if (prod instanceof Prato prato) {
          System.out.println("[DEBUG] Entrou no if Prato");
          for (Ingrediente ing : prato.getComposicao()) {
            ing.decrementarStock(item.getQuantidade());
            if (ing.getStock() <= 0) {
              ing.incrementarStock(10); // repõe 10 unidades
              System.out.println(
                  "[Stock] Ingrediente '"
                      + ing.getDesignacao()
                      + "' reabastecido para "
                      + ing.getStock()
                      + ".");
            }
          }
          for (int q = 0; q < item.getQuantidade(); q++) {
            for (Ingrediente ing : prato.getComposicao()) {
              System.out.printf(
                  "A preparar %s... Prima ENTER para continuar...", ing.getDesignacao());
              in.nextLine();
            }
          }
        } else if (prod instanceof Produto
            && (prod.getClass().getSimpleName().equals("Menu")
                || prod.getId().startsWith("MENU-"))) {
          System.out.println("[DEBUG] Entrou no if Menu");
          try {
            Menu menu = (Menu) prod;
            Prato prato = menu.getPrato();
            for (Ingrediente ing : prato.getComposicao()) {
              if (ing.getStock() <= 0) {
                ing.incrementarStock(10);
                System.out.println(
                    "[Stock] Ingrediente '"
                        + ing.getDesignacao()
                        + "' reabastecido para "
                        + ing.getStock()
                        + ".");
              }
            }
            System.out.println("[Menu] Preparar prato: " + prato.getDesignacao());
            for (int q = 0; q < item.getQuantidade(); q++) {
              for (Ingrediente ing : prato.getComposicao()) {
                System.out.printf(
                    "A preparar %s... Prima ENTER para continuar...", ing.getDesignacao());
                in.nextLine();
              }
            }
            Bebida bebida = menu.getBebida();
            if (bebida.getStock() <= 0) {
              bebida.setStock(10);
              System.out.println(
                  "[Stock] Bebida '" + bebida.getDesignacao() + "' reabastecida para 10.");
            }
            System.out.println("[Menu] Preparar bebida: " + bebida.getDesignacao());
            for (int q = 0; q < item.getQuantidade(); q++) {
              System.out.printf(
                  "A preparar %s... Prima ENTER para continuar...", bebida.getDesignacao());
              in.nextLine();
            }
          } catch (Exception e) {
            System.out.println("[DEBUG] Erro ao cast para Menu: " + e);
          }
        } else if (prod instanceof Bebida bebida) {
          if (bebida.getStock() <= 0) {
            bebida.setStock(10);
            System.out.println(
                "[Stock] Bebida '" + bebida.getDesignacao() + "' reabastecida para 10.");
          }
          System.out.println("[DEBUG] Entrou no else (bebida)");
          for (int q = 0; q < item.getQuantidade(); q++) {
            System.out.printf(
                "A preparar %s... Prima ENTER para continuar...", prod.getDesignacao());
            in.nextLine();
          }
        } else {
          System.out.println("[DEBUG] Entrou no else (outro produto)");
          for (int q = 0; q < item.getQuantidade(); q++) {
            System.out.printf(
                "A preparar %s... Prima ENTER para continuar...", prod.getDesignacao());
            in.nextLine();
          }
        }
      }
      System.out.println("Todos os itens preparados!");
    }

    System.out.print("\nDeseja marcar este pedido como pronto? (s/n): ");
    String confirma = in.nextLine().trim().toLowerCase();
    if (!confirma.equals("s")) {
      System.out.println("Operação cancelada. O pedido não foi marcado como pronto.");
      return;
    }

    boolean ok = ln.marcarPedidoPronto(idx);
    if (ok) {
      System.out.println("Pedido pronto para o cliente!");
      try {
        Pedido pedidoPronto = filaPedidos.get(idx);
        var lista = ln.listarEntregas();
        for (var entrega : lista) {
          if (entrega.getPedido() != null && entrega.getPedido().getId().equals(pedidoPronto.getId())) {
            if (entrega.getEstado() != EstadoEntrega.ENTREGUE) {
              entrega.setEstado(EstadoEntrega.PRONTO);
              ln.atualizarEntrega(entrega);
              System.out.println("Entrega marcada como pronta!");
            }
            break;
          }
        }
      } catch (Exception e) {
        System.out.println("[DEBUG] Falha ao marcar entrega como pronta: " + e.getMessage());
      }
    } else {
      System.out.println("Falha ao marcar.");
    }
  }

  private void removerPedido() {
    List<Pedido> filaPedidos = ln.listarPedidosFila();
    if (filaPedidos == null || filaPedidos.isEmpty()) {
      System.out.println("Fila vazia.");
      return;
    }
    listarFila();
    System.out.print("Índice do pedido a remover: ");
    int idx = UtilsMenu.readIntSafe(in, 1) - 1;
    if (idx < 0 || idx >= filaPedidos.size()) {
      System.out.println("Índice inválido.");
      return;
    }
    boolean ok = ln.removerPedidoDaFila(idx);
    System.out.println(ok ? "Pedido removido da fila." : "Falha a remover.");
  }

  private void confirmarPedido() {
    String idRest = ln.getRestauranteIdPorUsername(this.userName);
    List<Pedido> filaPedidosNaoConfirmados = ln.listarPedidosNaoConfirmados();
    if (filaPedidosNaoConfirmados == null || filaPedidosNaoConfirmados.isEmpty()) {
      System.out.println("Fila vazia.");
      return;
    }
    listarPedidosNaoConfirmados();
    System.out.print("ID do pedido a confirmar: ");
    String idPedido = in.nextLine().trim();
    boolean ok = ln.confirmarPedido(idPedido, idRest);
    System.out.println(ok ? "Pedido confirmado." : "Falha a confirmar pedido.");
  }

  private String autentica(String user, String pass) {
    return ln.login(user, pass);
  }

  private void listarPedidosNaoConfirmados() {
    try {
      List<Pedido> pedidos = ln.listarPedidosNaoConfirmados();
      if (pedidos == null || pedidos.isEmpty()) {
        System.out.println("Não há pedidos não confirmados.");
        return;
      }
      System.out.println("\n--- Pedidos Não Confirmados ---");
      for (Pedido p : pedidos) {
        System.out.printf(
            "- ID: %s | Pronto: %s | Pago: %s | Data: %s\n",
            p.getId(), p.isPronto(), p.isPago(), p.getDataHora());
      }
    } catch (Exception e) {
      System.out.println("Erro ao listar pedidos não confirmados: " + e.getMessage());
    }
  }
}

