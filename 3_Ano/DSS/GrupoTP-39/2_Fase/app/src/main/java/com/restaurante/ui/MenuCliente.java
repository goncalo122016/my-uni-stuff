package com.restaurante.ui;

import com.restaurante.business.IRestauranteLN;
import com.restaurante.business.gestPedidos.*;
import com.restaurante.business.gestEntregas.*;
import com.restaurante.business.gestProdutos.*;
import com.restaurante.utils.UtilsMenu;
import java.time.LocalDateTime;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Optional;
import java.util.Scanner;

public class MenuCliente {
  private final Scanner in;
  private final IRestauranteLN ln;

  public MenuCliente(Scanner in, IRestauranteLN ln) {
    this.in = in;
    this.ln = ln;
  }

  public void run() {
    while (true) {
      System.out.println("\n--- Cliente ---");
      System.out.println("1) Listar produtos");
      System.out.println("2) Fazer pedido");
      System.out.println("3) Fazer pedido de Menu (prato + bebida)");
      System.out.println("4) Consultar pedido");
      System.out.println("5) Pagar pedido");
      System.out.println("6) Recolher pedido");
      System.out.println("0) Voltar");
      System.out.print("> ");

      String opt = in.nextLine().trim();
      switch (opt) {
        case "1" -> listarProdutosCliente();
        case "2" -> criarPedidoCliente();
        case "3" -> criarPedidoMenuCliente();
        case "4" -> consultarPedidoCliente();
        case "5" -> pagarPedidoCliente();
        case "6" -> recolherPedidoCliente();
        case "0" -> {
          return;
        }
        default -> UtilsMenu.opcaoInvalida();
      }
    }
  }

  // Permite ao cliente criar um pedido de Menu (prato + bebida)
  private void criarPedidoMenuCliente() {
    try {
      // Buscar todos os pratos e bebidas
      List<Produto> produtos = Optional.ofNullable(ln.listarProdutos()).orElseGet(ArrayList::new);
      List<Prato> pratos = new ArrayList<>();
      List<Bebida> bebidas = new ArrayList<>();
      for (Produto p : produtos) {
        if (p instanceof Prato pr) pratos.add(pr);
        if (p instanceof Bebida b) bebidas.add(b);
      }
      // Buscar menus reais da base de dados
      List<Menu> menus = ln.listarMenus(pratos, bebidas);
      if (menus.isEmpty()) {
        System.out.println("Não há menus disponíveis para pedido.");
        return;
      }
      System.out.println("\n--- Criar Pedido de Menu (predefinido) ---");
      List<ItemPedido> items = new ArrayList<>();
      while (true) {
        System.out.println("Menus disponíveis:");
        for (int i = 0; i < menus.size(); i++) {
          Menu m = menus.get(i);
          System.out.printf("%d) %s (%.2f€)\n", i + 1, m.getDesignacao(), m.getPreco());
        }
        System.out.print("Escolha o número do menu (ou ENTER para terminar): ");
        String opt = in.nextLine().trim();
        if (opt.isEmpty()) break;
        int idxMenu;
        try {
          idxMenu = Integer.parseInt(opt) - 1;
        } catch (NumberFormatException ex) {
          System.out.println("Entrada inválida.");
          continue;
        }
        if (idxMenu < 0 || idxMenu >= menus.size()) {
          System.out.println("Menu inválido. Operação cancelada.");
          continue;
        }
        Menu menuEscolhido = menus.get(idxMenu);
        // Mostrar detalhes do menu
        System.out.println("--- Detalhes do Menu ---");
        System.out.println("Prato: " + menuEscolhido.getPrato().getDesignacao());
        if (menuEscolhido.getPrato().getComposicao() != null
            && !menuEscolhido.getPrato().getComposicao().isEmpty()) {
          System.out.print("  Ingredientes: ");
          var comp = menuEscolhido.getPrato().getComposicao();
          for (int i = 0; i < comp.size(); i++) {
            System.out.print(comp.get(i).getDesignacao());
            if (i < comp.size() - 1) System.out.print(", ");
          }
          System.out.println();
        }
        System.out.println("Bebida: " + menuEscolhido.getBebida().getDesignacao());
        System.out.print("Quantidade de menus: ");
        int qtd = UtilsMenu.readIntSafe(in, 1);
        if (qtd <= 0) qtd = 1;
        items.add(new ItemPedido(qtd, "", menuEscolhido, new ArrayList<>()));
        System.out.println("✓ Menu adicionado ao pedido.");
      }

      // Permitir remover menus do pedido antes de finalizar
      while (true) {
        if (items.isEmpty()) {
          System.out.println("Nenhum menu no pedido. Cancelado.");
          return;
        }
        System.out.print("Deseja remover algum componente do menu do pedido? (s/n): ");
        String resp = in.nextLine().trim().toLowerCase();
        if (resp.equals("s")) {
          // Listar todos os pratos e bebidas dos menus
          List<String> opcoes = new ArrayList<>();
          List<Integer> idxItem = new ArrayList<>();
          List<Boolean> isPrato = new ArrayList<>();
          int count = 1;
          for (int i = 0; i < items.size(); i++) {
            ItemPedido it = items.get(i);
            if (it.getProduto() instanceof Menu m) {
              opcoes.add(m.getPrato().getDesignacao());
              idxItem.add(i);
              isPrato.add(true);
              System.out.printf("%d) %s\n", count++, m.getPrato().getDesignacao());
              opcoes.add(m.getBebida().getDesignacao());
              idxItem.add(i);
              isPrato.add(false);
              System.out.printf("%d) %s\n", count++, m.getBebida().getDesignacao());
            } else {
              opcoes.add(it.getProduto().getDesignacao());
              idxItem.add(i);
              isPrato.add(true);
              System.out.printf("%d) %s\n", count++, it.getProduto().getDesignacao());
            }
          }
          System.out.print("Digite o número do componente a remover (ou 0 para cancelar): ");
          String idxStr = in.nextLine().trim();
          int idxRemover = -1;
          try {
            idxRemover = Integer.parseInt(idxStr) - 1;
          } catch (NumberFormatException ex) {
            System.out.println("Entrada inválida.");
            continue;
          }
          if (idxRemover == -1) break;
          if (idxRemover < 0 || idxRemover >= opcoes.size()) {
            System.out.println("Índice inválido.");
            continue;
          }
          int itemIdx = idxItem.get(idxRemover);
          ItemPedido it = items.get(itemIdx);
          if (it.getProduto() instanceof Menu m) {
            ItemPedido novoItem;
            if (isPrato.get(idxRemover)) {
              // Remover prato: adicionar só bebida
              novoItem =
                  new ItemPedido(
                      it.getQuantidade(), it.getNotas(), m.getBebida(), new ArrayList<>());
              System.out.println("Prato removido do menu. Ficou só a bebida.");
            } else {
              // Remover bebida: adicionar só prato
              novoItem =
                  new ItemPedido(
                      it.getQuantidade(), it.getNotas(), m.getPrato(), new ArrayList<>());
              System.out.println("Bebida removida do menu. Ficou só o prato.");
            }
            items.remove(itemIdx);
            items.add(novoItem);
          } else {
            // Produto simples
            items.remove(itemIdx);
            System.out.println("Item removido.");
          }

          // Atualizar e mostrar o total após remoção
          double total = 0.0;
          for (ItemPedido item : items) {
            total += item.getProduto().getPreco() * item.getQuantidade();
          }
          System.out.printf("Total atual: %.2f€\n", total);

        } else {
          break;
        }
      }

      String idPedido = "P-" + (100 + (int) (Math.random() * 900));
      Pedido pedido = new Pedido(idPedido, false, LocalDateTime.now(), false, items);
      ln.registarPedido(pedido);
      System.out.println("✓ Pedido de Menu criado com sucesso! ID: " + pedido.getId());
      System.out.printf("Total: %.2f€\n", pedido.calculaTotal());
    } catch (Exception e) {
      System.out.println("Erro ao obter menus: " + e.getMessage());
    }
  }

  private void listarProdutosCliente() {
    try {
      List<Produto> produtos = ln.listarProdutos();
      if (produtos == null || produtos.isEmpty()) {
        System.out.println("Sem produtos disponíveis.");
        return;
      }
      System.out.println("\nProdutos disponíveis:");
      for (Produto p : produtos) {
        System.out.printf(
            "- [%s] %s | %.2f€ | %s\n",
            p.getId(),
            p.getDesignacao(),
            p.getPreco(),
            p.isDisponivel() ? "disponível" : "indisponível");

        // Detalhes adicionais para pratos: composição e opções
        if (p instanceof Prato) {
          Prato prato = (Prato) p;
          List<Ingrediente> comp = prato.getComposicao();
          if (comp != null && !comp.isEmpty()) {
            StringBuilder sb = new StringBuilder();
            for (int i = 0; i < comp.size(); i++) {
              Ingrediente ing = comp.get(i);
              sb.append(ing.getDesignacao());
              if (i < comp.size() - 1) sb.append(", ");
            }
            System.out.println("  composição: " + sb);
          } else {
            System.out.println("  composição: (nenhuma)");
          }

          List<Ingrediente> opcoes = prato.getOpcoes();
          if (opcoes != null && !opcoes.isEmpty()) {
            StringBuilder so = new StringBuilder();
            for (int i = 0; i < opcoes.size(); i++) {
              so.append(opcoes.get(i).getDesignacao());
              if (i < opcoes.size() - 1) so.append(", ");
            }
            System.out.println("  opções: " + so);
          } else {
            System.out.println("  opções: (nenhuma)");
          }
        }
      }
    } catch (Exception e) {
      System.out.println("Erro ao listar produtos: " + e.getMessage());
    }
  }

  private void criarPedidoCliente() {
    // Mostra produtos para facilitar
    List<Produto> produtos = Optional.ofNullable(ln.listarProdutos()).orElseGet(ArrayList::new);
    Map<String, Produto> produtosById = new HashMap<>();
    if (produtos.isEmpty()) {
      System.out.println(
          "Nota: lista de produtos indisponível.\n"
              + "Pode introduzir IDs manualmente, serão registados sem validação.");
    } else {
      System.out.println("\nProdutos:");
      for (Produto p : produtos) {
        produtosById.put(p.getId(), p);
        // Também adiciona versão simplificada (ex: "001" para "PROD001")
        if (p.getId().startsWith("PROD")) {
          produtosById.put(p.getId().substring(4), p);
        }
        System.out.printf("- [%s] %s | %.2f€\n", p.getId(), p.getDesignacao(), p.getPreco());
      }
    }

    List<ItemPedido> items = new ArrayList<>();
    while (true) {
      System.out.print("ID do produto (ou ENTER para terminar): ");
      String id = in.nextLine().trim();
      if (id.isEmpty()) break;

      Produto prod = produtosById.get(id);
      if (prod == null) prod = produtosById.get("PROD" + id);
      if (prod == null) {
        System.out.println("Produto não encontrado. Tente novamente.");
        continue;
      }

      int qtd = 1;
      System.out.print("Quantidade: ");
      qtd = UtilsMenu.readIntSafe(in, 1);

      List<Ingrediente> extras = new ArrayList<>();
      if (prod instanceof Prato prato) {
        // Mostrar opções disponíveis
        List<Ingrediente> ext = prato.getOpcoes();
        if (ext != null && !ext.isEmpty()) {
          System.out.println("Opções disponíveis para adicionar:");
          for (int i = 0; i < ext.size(); i++) {
            Ingrediente opt = ext.get(i);
            System.out.printf("%d) %s (+%.2f€)\n", i + 1, opt.getDesignacao(), opt.getPreco());
          }
          // Acrescentar ingredientes extra
          System.out.print(
              "Deseja acrescentar alguma opção? (separar por números ou ENTER para nenhum): ");
          String extra = in.nextLine().trim();
          if (!extra.isEmpty()) {
            String[] extraArr = extra.split(",");
            for (String e : extraArr) {
              try {
                int idx = Integer.parseInt(e.trim()) - 1;
                if (idx >= 0 && idx < ext.size()) {
                  Ingrediente opcao = ext.get(idx);
                  extras.add(
                      new Ingrediente(opcao.getStock(), opcao.getDesignacao(), opcao.getPreco()));
                } else {
                  System.out.println("Opção inválida: " + (idx + 1));
                }
              } catch (NumberFormatException ex) {
                System.out.println("Número inválido: " + e.trim());
              }
            }
          }
        }
      }
      // Perguntar nota para a cozinha
      System.out.print("Alguma nota para a cozinha? (ENTER para nenhuma): ");
      String notaCozinha = in.nextLine().trim();
      ItemPedido item = new ItemPedido(qtd, notaCozinha, prod, extras);
      items.add(item);
      double precoFinal = prod.getPreco() * qtd + extras.size() * 1.00 * qtd;
      System.out.printf("✓ Adicionado: %dx %s (%.2f€)\n", qtd, prod.getDesignacao(), precoFinal);

      // Permitir remover itens do pedido
      while (true) {
        System.out.print("Deseja remover algum item do pedido? (s/n): ");
        String resp = in.nextLine().trim().toLowerCase();
        if (resp.equals("s")) {
          if (items.isEmpty()) {
            System.out.println("Nenhum item para remover.");
            break;
          }
          System.out.println("Itens atuais no pedido:");
          for (int i = 0; i < items.size(); i++) {
            ItemPedido it = items.get(i);
            System.out.printf(
                "%d) %s x%d\n", i + 1, it.getProduto().getDesignacao(), it.getQuantidade());
          }
          System.out.print("Digite o número do item a remover (ou 0 para cancelar): ");
          String idxStr = in.nextLine().trim();
          int idxRemover = -1;
          try {
            idxRemover = Integer.parseInt(idxStr) - 1;
          } catch (NumberFormatException ex) {
            System.out.println("Entrada inválida.");
            continue;
          }
          if (idxRemover == -1) break;
          if (idxRemover < 0 || idxRemover >= items.size()) {
            System.out.println("Índice inválido.");
            continue;
          }
          items.remove(idxRemover);
          System.out.println("Item removido.");
        } else {
          break;
        }
      }
    }

    if (items.isEmpty()) {
      System.out.println("Pedido sem itens. Cancelado.");
      return;
    }

    String idPedido = "P-" + (100 + (int) (Math.random() * 900));
    Pedido pedido = new Pedido(idPedido, false, LocalDateTime.now(), false, items);

    try {
      ln.registarPedido(pedido); // poderá ser no-op até backend estar implementado
    } catch (Exception e) {
      System.out.println("Aviso: não foi possível registar no backend: " + e.getMessage());
    }

    System.out.println("Pedido criado com ID: " + pedido.getId());
    System.out.printf("Total: %.2f€\n", pedido.calculaTotal());
  }

  private void consultarPedidoCliente() {
    System.out.print("ID do pedido: ");
    String idPedido = in.nextLine().trim();
    if (idPedido.isEmpty()) {
      System.out.println("ID inválido.");
      return;
    }

    Pedido pedido = ln.consultarPedido(idPedido);
    if (pedido == null) {
      System.out.println("Pedido não encontrado.");
      return;
    }

    System.out.println("\n=== Detalhes do Pedido ===");
    System.out.println("ID: " + pedido.getId());
    System.out.println("Data/Hora: " + pedido.getDataHora().toString().replace("T", " "));
    System.out.println("Estado: " + (pedido.isPronto() ? "Pronto" : "Em preparação"));
    System.out.println("Pagamento: " + (pedido.isPago() ? "✓ Pago" : "✗ Pendente"));
    // Mostrar estado da entrega associada, se existir
    List<Entrega> todas = ln.listarEntregas();
    Entrega entrega =
        todas.stream()
            .filter(e -> e.getPedido() != null && pedido.getId().equals(e.getPedido().getId()))
            .findFirst()
            .orElse(null);
    if (entrega != null) {
      System.out.println("Entrega: " + entrega.getEstado());
    }

    // Simulação visual de preparação (mas não altera estado)
    if (!pedido.isPronto()) {
      System.out.print("\n[Preparação] O seu pedido está a ser preparado");
      try {
        for (int i = 0; i < 3; i++) {
          Thread.sleep(700);
          System.out.print(".");
        }
      } catch (InterruptedException e) {
        // Ignorar
      }
      System.out.println(
          "\nAinda em preparação. Por favor aguarde que um funcionário marque como pronto.");
    }

    List<ItemPedido> items = pedido.getItems();
    if (items != null && !items.isEmpty()) {
      System.out.println("\nItens:");
      for (ItemPedido item : items) {
        if (item.getProduto() != null) {
          System.out.printf(
              "  - %dx %s (%.2f€)\n",
              item.getQuantidade(), item.getProduto().getDesignacao(), item.calculaTotal());
          // Ingredientes base (menos removidos)
          if (item.getProduto() instanceof Prato prato) {
            List<String> removidos = new ArrayList<>();
            if (item.getNotas() != null && item.getNotas().startsWith("Sem: ")) {
              String[] parts = item.getNotas().substring(5).split(",");
              for (String s : parts) removidos.add(s.trim());
            }
            System.out.print("    Base: ");
            List<Ingrediente> comp = prato.getComposicao();
            boolean first = true;
            for (Ingrediente ing : comp) {
              if (!removidos.contains(ing.getDesignacao())) {
                if (!first) System.out.print(", ");
                System.out.print(ing.getDesignacao());
                first = false;
              }
            }
            System.out.println();
          }
          // Ingredientes extra
          if (item.getExtras() != null && !item.getExtras().isEmpty()) {
            System.out.print("    Extras: ");
            for (int i = 0; i < item.getExtras().size(); i++) {
              var ing = item.getExtras().get(i);
              System.out.printf("%s (+%.2f€)", ing.getDesignacao(), ing.getPreco());
              if (i < item.getExtras().size() - 1) System.out.print(", ");
            }
            System.out.println();
          }
          // Ingredientes removidos
          if (item.getNotas() != null && item.getNotas().startsWith("Sem: ")) {
            System.out.print("    Removidos: ");
            String[] parts = item.getNotas().substring(5).split(",");
            for (int i = 0; i < parts.length; i++) {
              System.out.print(parts[i].trim());
              if (i < parts.length - 1) System.out.print(", ");
            }
            System.out.println();
          }
        }
      }
    }
    System.out.printf("\nTotal: %.2f€\n", pedido.calculaTotal());
  }

  private void pagarPedidoCliente() {
    System.out.print("ID do pedido: ");
    String idPedido = in.nextLine().trim();
    if (idPedido.isEmpty()) {
      System.out.println("ID inválido.");
      return;
    }

    Pedido pedido = ln.consultarPedido(idPedido);
    if (pedido == null) {
      System.out.println("Pedido não encontrado.");
      return;
    }

    if (pedido.isPago()) {
      System.out.println("Este pedido já está pago.");
      return;
    }

    double total = ln.calcularTotalPedido(idPedido);
    System.out.printf("\nTotal a pagar: %.2f€\n", total);

    System.out.println("\nMétodo de pagamento:");
    System.out.println("1) Dinheiro");
    System.out.println("2) MBWay");
    System.out.println("3) Multibanco");
    System.out.println("0) Cancelar");
    System.out.print("> ");

    String opt = in.nextLine().trim();
    MetodoPagamento metodo;
    switch (opt) {
      case "1":
        metodo = MetodoPagamento.DINHEIRO;
        break;
      case "2":
        metodo = MetodoPagamento.MBWAY;
        break;
      case "3":
        metodo = MetodoPagamento.MULTIBANCO;
        break;
      case "0":
        System.out.println("Pagamento cancelado.");
        return;
      default:
        System.out.println("Opção inválida.");
        return;
    }

    boolean ok = ln.processarPagamento(idPedido, metodo);
    if (ok) {
      System.out.println("✓ Pagamento efetuado com sucesso!");
      System.out.printf("Método: %s | Valor: %.2f€\n", metodo, total);

      // Perguntar método de entrega
      System.out.println("\nMétodo de entrega:");
      System.out.println("1) Balcão (levantar no restaurante)");
      System.out.println("2) Delivery (entrega ao domicílio)");
      System.out.print("> ");
      String optEntrega = in.nextLine().trim();
      TipoEntrega tipoEntrega;
      String endereco = null;
      switch (optEntrega) {
        case "1" -> tipoEntrega = TipoEntrega.BALCAO;
        case "2" -> {
          tipoEntrega = TipoEntrega.DELIVERY;
          System.out.print("Endereço para entrega: ");
          endereco = in.nextLine().trim();
          if (endereco.isEmpty()) {
            System.out.println("Endereço obrigatório para delivery. Operação cancelada.");
            return;
          }
        }
        default -> {
          System.out.println("Opção inválida. Operação cancelada.");
          return;
        }
      }

      // Criar entrega com estado PENDENTE
      try {
        Pedido pedidoPago = ln.consultarPedido(idPedido);
        ln.criarEntregaCustomizada(pedidoPago, tipoEntrega, endereco);
        System.out.println("Entrega criada e marcada como PENDENTE.");
      } catch (Exception e) {
        System.out.println("[DEBUG] Falha ao criar entrega: " + e.getMessage());
      }
    } else {
      System.out.println("✗ Falha ao processar pagamento.");
    }
  }

  private void recolherPedidoCliente() {
    System.out.print("ID do pedido a recolher: ");
    String idPedido = in.nextLine().trim();
    if (idPedido.isEmpty()) {
      System.out.println("ID inválido.");
      return;
    }

    Pedido pedido = ln.consultarPedido(idPedido);
    if (pedido == null) {
      System.out.println("Pedido não encontrado.");
      return;
    }

    // Buscar entrega associada
    List<Entrega> todas = ln.listarEntregas();
    Entrega entrega =
        todas.stream()
            .filter(e -> e.getPedido() != null && idPedido.equals(e.getPedido().getId()))
            .findFirst()
            .orElse(null);

    System.out.println("\nPedido encontrado:");
    System.out.printf("ID: %s\n", pedido.getId());
    System.out.printf("Total: %.2f€\n", pedido.calculaTotal());
    System.out.printf("Pronto: %s\n", pedido.isPronto() ? "Sim" : "Não");
    System.out.printf("Pago: %s\n", pedido.isPago() ? "Sim" : "Não");
    if (entrega != null) {
      System.out.printf("Estado da entrega: %s\n", entrega.getEstado());
    }

    if (!pedido.isPronto()) {
      System.out.println("\n✗ O pedido ainda não está pronto para recolher.");
      return;
    }

    if (!pedido.isPago()) {
      System.out.println("\n✗ O pedido ainda não foi pago. Pague primeiro antes de recolher.");
      return;
    }

    if (entrega != null
        && entrega.getEstado() == EstadoEntrega.ENTREGUE) {
      System.out.println("\n✗ Este pedido já foi recolhido anteriormente.");
      return;
    }

    boolean ok = ln.recolherPedido(idPedido);
    if (ok) {
      System.out.println("\n✓ Pedido recolhido com sucesso!");
      System.out.println("Obrigado pela preferência!");
    } else {
      System.out.println("\n✗ Não foi possível recolher o pedido.");
    }
  }
}
