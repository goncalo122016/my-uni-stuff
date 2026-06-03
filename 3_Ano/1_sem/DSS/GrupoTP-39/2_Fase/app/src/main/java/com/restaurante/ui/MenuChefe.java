package com.restaurante.ui;

import com.restaurante.business.IRestauranteLN;
import com.restaurante.business.gestEstatisticas.*;
import com.restaurante.business.gestPedidos.*;
import com.restaurante.business.gestProdutos.*;
import com.restaurante.business.gestEntregas.*;
import com.restaurante.utils.UtilsMenu;

import java.util.List;
import java.util.Scanner;

public class MenuChefe {
  private final Scanner in;
  private final IRestauranteLN ln;
  private String username;

  public MenuChefe(Scanner in, IRestauranteLN ln) {
    this.in = in;
    this.ln = ln;
  }

  public void run() {
    System.out.print("Utilizador: ");
    String user = in.nextLine().trim();
    System.out.print("Password: ");
    String pass = in.nextLine().trim();

    if (autentica(user, pass) == null || !autentica(user, pass).equals("CHEFE")) {
      UtilsMenu.erro("❌ Credenciais inválidas.");
      return;
    }
    this.username = user;

    System.out.println("✓ Login bem-sucedido!");

    while (true) {
      System.out.println("\n--- Menu Chefe ---");
      System.out.println("1) Estatísticas do restaurante");
      System.out.println("2) Listar pedidos do restaurante");
      System.out.println("3) Listar produtos");
      System.out.println("4) Ver entregas do restaurante");
      System.out.println("0) Sair");
      System.out.print("> ");

      String opt = in.nextLine().trim();
      switch (opt) {
        case "1" -> verEstatisticasRestaurante();
        case "2" -> listarPedidosRestaurante();
        case "3" -> listarTodosProdutosChefe();
        case "4" -> verEntregasRestaurante();
        case "0" -> {
          return;
        }
        default -> UtilsMenu.opcaoInvalida();
      }
    }
  }

  private void verEstatisticasRestaurante() {
    String idRest = ln.getRestauranteIdPorUsername(this.username);

    List<Restaurante> restaurantes = ln.listarRestaurantes();
    boolean existe = restaurantes.stream().anyMatch(r -> r.getId().equals(idRest));
    if (!existe) {
      System.out.println("Restaurante com ID '" + idRest + "' não existe.");
      return;
    }
    Estatisticas est = ln.getEstatisticasRestaurante(idRest);
    System.out.println("\n=== Estatísticas do Restaurante ===");
    System.out.println("Tipo: " + est.getTipo());
    System.out.println("Total pedidos: " + est.getTotalPedidos());
    System.out.printf("Total receita: %.2f€\n", est.getTotalReceita());
    // Mostrar pedidos do restaurante
    List<Pedido> pedidos = ln.listarPedidosPorRestaurante(idRest);
    if (pedidos.isEmpty()) {
      System.out.println("Nenhum pedido encontrado para este restaurante.");
    } else {
      System.out.println("Pedidos deste restaurante:");
      for (Pedido p : pedidos) {
        System.out.printf(
            "- Pedido %s | Total: %.2f€ | Pronto: %s | Pago: %s\n",
            p.getId(),
            p.calculaTotal(),
            p.isPronto() ? "Sim" : "Não",
            p.isPago() ? "Sim" : "Não");
      }
    }
  }

  private void listarPedidosRestaurante() {
    String idRest = ln.getRestauranteIdPorUsername(this.username);

    List<Pedido> pedidos = ln.listarPedidosPorRestaurante(idRest);
    if (pedidos == null || pedidos.isEmpty()) {
      System.out.println("Sem pedidos registados.");
      return;
    }
    System.out.println("\n=== Todos os Pedidos ===");
    System.out.printf(
        "%-15s %-8s %-8s %-20s %-10s\n", "ID", "Pronto", "Pago", "Data/Hora", "Total");
    System.out.println("-".repeat(65));
    for (Pedido p : pedidos) {
      System.out.printf(
          "%-15s %-8s %-8s %-20s %.2f€\n",
          p.getId(),
          p.isPronto() ? "Sim" : "Não",
          p.isPago() ? "Sim" : "Não",
          p.getDataHora().toString().replace("T", " "),
          p.calculaTotal());
    }
    System.out.println("-".repeat(65));
    System.out.println("Total de pedidos: " + pedidos.size());
  }

  private void verEntregasRestaurante() {
    String idRest = ln.getRestauranteIdPorUsername(this.username);
    try {
      List<Entrega> lista = ln.listarEntregasPorRestaurante(idRest);
      if (lista == null || lista.isEmpty()) {
        System.out.println("Sem entregas registadas.");
        return;
      }
      System.out.println("\n=== Todas as Entregas ===");
      System.out.printf(
          "%-15s %-12s %-10s %-20s %-20s %-10s\n",
          "ID", "Pedido", "Tipo", "Estado", "Data Criação", "Endereço");
      System.out.println("-".repeat(90));
      for (var e : lista) {
        System.out.printf(
            "%-15s %-12s %-10s %-20s %-20s %-10s\n",
            e.getId(),
            e.getPedido() != null ? e.getPedido().getId() : "-",
            e.getTipo(),
            e.getEstado(),
            e.getDataHora() != null ? e.getDataHora().toString().replace("T", " ") : "",
            e.getEndereco() != null ? e.getEndereco() : "-");
      }
      System.out.println("-".repeat(90));
      System.out.println("Total de entregas: " + lista.size());
    } catch (Exception e) {
      System.out.println("Erro ao obter entregas: " + e.getMessage());
    }
  }

  /**
   * Autentica um utilizador e retorna o cargo se válido, null caso contrário.
   * Utilizadores válidos:
   * - admin/admin (Administrador) - chefe/chefe123 (Chefe de Cozinha) -
   * gerente/gerente123
   * (Gerente)
   */
  private String autentica(String user, String pass) {
    return ln.login(user, pass);
  }

  private void listarTodosProdutosChefe() {
    List<Produto> produtos = ln.listarProdutos();
    if (produtos == null || produtos.isEmpty()) {
      System.out.println("Sem produtos registados.");
      return;
    }

    System.out.println("\n=== Produtos Disponíveis ===");
    System.out.printf("%-10s %-25s %-10s %-12s\n", "ID", "Designação", "Preço", "Disponível");
    System.out.println("-".repeat(60));

    for (Produto p : produtos) {
      System.out.printf(
          "%-10s %-25s %.2f€      %-12s\n",
          p.getId(),
          p.getDesignacao().length() > 25
              ? p.getDesignacao().substring(0, 22) + "..."
              : p.getDesignacao(),
          p.getPreco(),
          p.isDisponivel() ? "Sim" : "Não");
    }
    System.out.println("-".repeat(60));
    System.out.println("Total de produtos: " + produtos.size());
  }
}
