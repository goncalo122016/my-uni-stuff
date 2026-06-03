package com.restaurante.data;

import com.restaurante.business.gestPedidos.*;
import com.restaurante.business.gestProdutos.*;
import com.restaurante.data.dao.PedidoDAO;
import com.restaurante.data.dao.ProdutoDAO;
import java.time.LocalDateTime;
import java.util.ArrayList;
import java.util.List;
import java.util.UUID;

/** Classe exemplo de como usar os DAOs e o DatabaseManager */
public class ExemploUso {

  public static void main(String[] args) {
    try {
      // ==========================================
      // 1. INICIALIZAR A BASE DE DADOS
      // ==========================================
      System.out.println("=== Inicializando Base de Dados ===\n");
      DatabaseManager dbManager = DatabaseManager.getInstance();
      dbManager.initializeDatabase();

      // ==========================================
      // 2. TRABALHAR COM PRODUTOS
      // ==========================================
      System.out.println("\n=== Testando ProdutoDAO ===\n");
      ProdutoDAO produtoDAO = new ProdutoDAO();

      // Listar todos os produtos
      List<Produto> todosProdutos = produtoDAO.readAll();
      System.out.println("Total de produtos: " + todosProdutos.size());
      for (Produto p : todosProdutos) {
        System.out.println("  - " + p.getDesignacao() + " (€" + p.getPreco() + ")");
      }

      // Listar apenas pratos
      System.out.println("\nPratos disponíveis:");
      List<Prato> pratos = produtoDAO.readAllPratos();
      for (Prato p : pratos) {
        System.out.println("  - " + p.getDesignacao() + " (€" + p.getPreco() + ")");
      }

      // Listar apenas bebidas
      System.out.println("\nBebidas disponíveis:");
      List<Bebida> bebidas = produtoDAO.readAllBebidas();
      for (Bebida b : bebidas) {
        System.out.println("  - " + b.getDesignacao() + " (€" + b.getPreco() + ")");
      }

      // Obter um produto específico
      System.out.println("\nBuscando produto PROD001:");
      Produto p1 = produtoDAO.read("PROD001");
      if (p1 != null) {
        System.out.println("  Encontrado: " + p1.getDesignacao() + " - €" + p1.getPreco());
      }

      // ==========================================
      // 3. CRIAR UM NOVO PRODUTO
      // ==========================================
      System.out.println("\n=== Criando novo produto ===\n");
      String produtoId = "PROD999_" + UUID.randomUUID().toString().substring(0, 8);
      Bebida novaBebida = new Bebida(produtoId, "Suco de Maçã Premium", 5.99, true, 0.5f, 50);
      produtoDAO.create(novaBebida);
      System.out.println("✓ Nova bebida criada: " + novaBebida.getDesignacao());

      // Verificar que foi criada
      Bebida bebidaCriada = (Bebida) produtoDAO.read(produtoId);
      if (bebidaCriada != null) {
        System.out.println("✓ Confirmado: Bebida existe na BD!");
      }

      // ==========================================
      // 4. TRABALHAR COM PEDIDOS
      // ==========================================
      System.out.println("\n=== Testando PedidoDAO ===\n");
      PedidoDAO pedidoDAO = new PedidoDAO();

      // Listar todos os pedidos
      List<Pedido> todosPedidos = pedidoDAO.readAll();
      System.out.println("Total de pedidos na BD: " + todosPedidos.size());
      for (Pedido pedido : todosPedidos) {
        System.out.println(
            "  - ID: "
                + pedido.getId()
                + " | Pronto: "
                + pedido.isPronto()
                + " | Pago: "
                + pedido.isPago());
      }

      // Obter pedidos prontos
      System.out.println("\nPedidos prontos:");
      List<Pedido> prontos = pedidoDAO.readPedidosProntos();
      System.out.println("  Total: " + prontos.size());

      // Obter pedidos não pagos
      System.out.println("\nPedidos não pagos:");
      List<Pedido> naoPagos = pedidoDAO.readPedidosNaoPagos();
      for (Pedido pedido : naoPagos) {
        System.out.println("  - ID: " + pedido.getId() + " | Total: €" + pedido.calculaTotal());
      }

      // ==========================================
      // 5. CRIAR UM NOVO PEDIDO
      // ==========================================
      System.out.println("\n=== Criando novo pedido ===\n");
      String pedidoId = "PED" + System.currentTimeMillis();
      Pedido novoPedido =
          new Pedido(pedidoId, false, LocalDateTime.now(), false, new ArrayList<>());
      pedidoDAO.create(novoPedido);
      System.out.println("✓ Novo pedido criado: " + pedidoId);

      // Marcar como pronto
      novoPedido.setPronto(true);
      pedidoDAO.update(novoPedido);
      System.out.println("✓ Pedido marcado como pronto");

      // ==========================================
      // 6. LIMPEZA (OPCIONAL)
      // ==========================================
      // Descomente para limpar a BD
      // System.out.println("\n=== Limpando Base de Dados ===\n");
      // dbManager.clearDatabase();
      // System.out.println("✓ Base de dados limpa!");

      System.out.println("\n=== Teste Completo! ===\n");

    } catch (Exception e) {
      System.err.println("Erro durante execução: " + e.getMessage());
      e.printStackTrace();
    }
  }
}
