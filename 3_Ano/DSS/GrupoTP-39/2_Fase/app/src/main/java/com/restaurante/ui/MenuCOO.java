package com.restaurante.ui;

import com.restaurante.business.IRestauranteLN;
import com.restaurante.business.gestEstatisticas.Estatisticas;
import com.restaurante.business.gestEstatisticas.Restaurante;
import com.restaurante.business.gestPedidos.Pedido;
import com.restaurante.business.gestProdutos.Produto;
import com.restaurante.utils.UtilsMenu;
import java.util.List;
import java.util.Scanner;

public class MenuCOO {
    private final Scanner in;
    private final IRestauranteLN ln;

    public MenuCOO(Scanner in, IRestauranteLN ln) {
        this.in = in;
        this.ln = ln;
    }

    public void run() {
        System.out.print("Utilizador: ");
        String user = in.nextLine().trim();
        System.out.print("Password: ");
        String pass = in.nextLine().trim();

        if (autentica(user, pass) == null || !autentica(user, pass).equals("COO")) {
            UtilsMenu.erro("❌ Credenciais inválidas.");
            return;
        }

        while (true) {
            System.out.println("\n--- Menu Chefes ---");
            System.out.println("1) Estatísticas cadeia");
            System.out.println("2) Estatísticas restaurante");
            System.out.println("3) Listar pedidos");
            System.out.println("4) Listar produtos");
            System.out.println("5) Listar restaurantes");
            System.out.println("6) Ver todas as entregas");
            System.out.println("0) Sair");
            System.out.print("> ");

            String opt = in.nextLine().trim();
            switch (opt) {
                case "1" -> verEstatisticasCadeia();
                case "2" -> verEstatisticasRestaurante();
                case "3" -> listarTodosPedidos();
                case "4" -> listarTodosProdutosChefe();
                case "5" -> listarRestaurantesEIds();
                case "6" -> verTodasEntregas();
                case "0" -> {
                    return;
                }
                default -> UtilsMenu.opcaoInvalida();
            }
        }
    }

    // Estatística: ver todas as entregas
    private void verTodasEntregas() {
        try {
            com.restaurante.business.gestEntregas.GestEntregasFacade entregas = new com.restaurante.business.gestEntregas.GestEntregasFacade();
            List<com.restaurante.business.gestEntregas.Entrega> lista = entregas.listarEntregas();
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

    private void verEstatisticasCadeia() {
        try {
            Estatisticas est = ln.getEstatisticas();
            if (est == null) {
                System.out.println("Estatísticas indisponíveis.");
                return;
            }
            System.out.println("\n=== Estatísticas da Cadeia ===");
            System.out.println("Tipo: " + est.getTipo());
            System.out.println("Total pedidos: " + est.getTotalPedidos());
            System.out.printf("Total receita: %.2f€\n", est.getTotalReceita());
        } catch (Exception e) {
            System.out.println("Erro a obter estatísticas: " + e.getMessage());
        }
    }

    private void verEstatisticasRestaurante() {
        System.out.print("ID do restaurante (ou ENTER para geral): ");
        String idRestInput = in.nextLine().trim();
        if (idRestInput.isEmpty()) {
            verEstatisticasCadeia();
            return;
        }
        // Permitir inserir só o sufixo (ex: 001) ou o ID completo (REST001)
        String idRest = idRestInput.matches("^REST\\d{3}$")
                ? idRestInput
                : "REST" + String.format("%03d", Integer.parseInt(idRestInput.replaceAll("\\D", "")));
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

    private void listarTodosPedidos() {
        List<Pedido> pedidos = ln.listarTodosPedidos();
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

    private void listarTodosProdutosChefe() {
        List<Produto> produtos = ln.listarProdutos();
        if (produtos == null || produtos.isEmpty()) {
            System.out.println("Sem produtos registados.");
            return;
        }
        System.out.println("\n=== Todos os Produtos ===");
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

    private void listarRestaurantesEIds() {
    System.out.println("\n=== Restaurantes ===");
    List<Restaurante> restaurantes = ln.listarRestaurantes();
    if (restaurantes.isEmpty()) {
      System.out.println("Nenhum restaurante encontrado na base de dados.");
    } else {
      for (Restaurante r : restaurantes) {
        System.out.printf("ID: %s | %s\n", r.getId(), r.getNome());
      }
    }
  }
}

