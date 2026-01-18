package com.restaurante.data.dao;

import java.sql.Connection;
import java.sql.PreparedStatement;
import java.sql.ResultSet;
import java.sql.Statement;
import java.time.LocalDateTime;
import java.time.format.DateTimeFormatter;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import com.restaurante.business.gestPedidos.*;
import com.restaurante.business.gestProdutos.*;
import com.restaurante.data.DatabaseManager;

/**
 * Data Access Object para Pedidos
 * Implementa a interface Map para permitir acesso direto aos dados
 */
public class PedidoDAO extends HashMap<String, Pedido> implements IGenericDAO<Pedido> {

  private final DatabaseManager dbManager;

  public PedidoDAO() {
    this.dbManager = DatabaseManager.getInstance();
  }

  /** Busca todos os pedidos de um restaurante específico */
  public List<Pedido> readByRestaurante(String restauranteId) throws Exception {
    String sqlPedidos = "SELECT id, data_hora, pronto, pago, metodo_pagamento, total, restaurante_id FROM pedido WHERE restaurante_id = ?";
    String sqlItens = "SELECT pedido_id, produto_id, quantidade FROM item_pedido";

    List<Pedido> pedidos = new ArrayList<>();

    // Passo 1: Carregar pedidos do restaurante
    List<Map<String, Object>> pedidoRows = new ArrayList<>();
    try (Connection conn = dbManager.getConnection();
        PreparedStatement pstmt = conn.prepareStatement(sqlPedidos)) {

      pstmt.setString(1, restauranteId);

      try (ResultSet rs = pstmt.executeQuery()) {
        while (rs.next()) {
          Map<String, Object> row = new HashMap<>();
          row.put("id", rs.getString("id"));
          row.put("data_hora", rs.getString("data_hora"));
          row.put("pronto", rs.getInt("pronto"));
          row.put("pago", rs.getInt("pago"));
          row.put("total", rs.getDouble("total"));
          row.put("restaurante_id", rs.getString("restaurante_id"));
          pedidoRows.add(row);
        }
      }
    }

    // Passo 2: Carregar todos os itens
    List<Map<String, Object>> itemRows = new ArrayList<>();
    try (Connection conn = dbManager.getConnection();
        Statement stmt = conn.createStatement();
        ResultSet rs = stmt.executeQuery(sqlItens)) {

      while (rs.next()) {
        Map<String, Object> row = new HashMap<>();
        row.put("pedido_id", rs.getString("pedido_id"));
        row.put("produto_id", rs.getString("produto_id"));
        row.put("quantidade", rs.getInt("quantidade"));
        itemRows.add(row);
      }
    }

    // Passo 3: Construir objetos
    ProdutoDAO produtoDAO = new ProdutoDAO();
    for (Map<String, Object> pedidoRow : pedidoRows) {
      String id = (String) pedidoRow.get("id");
      String dataHoraStr = (String) pedidoRow.get("data_hora");

      LocalDateTime dataHora;
      try {
        dataHora = LocalDateTime.parse(dataHoraStr);
      } catch (Exception e) {
        try {
          DateTimeFormatter formatter = DateTimeFormatter.ofPattern("yyyy-MM-dd HH:mm:ss");
          dataHora = LocalDateTime.parse(dataHoraStr, formatter);
        } catch (Exception e2) {
          dataHora = LocalDateTime.now();
        }
      }

      boolean pronto = (int) pedidoRow.get("pronto") == 1;
      boolean pago = (int) pedidoRow.get("pago") == 1;
      double total = (double) pedidoRow.get("total");
      // Encontrar itens deste pedido
      List<com.restaurante.business.gestPedidos.ItemPedido> itens = new ArrayList<>();
      for (Map<String, Object> itemRow : itemRows) {
        if (id.equals(itemRow.get("pedido_id"))) {
          String produtoId = (String) itemRow.get("produto_id");
          int quantidade = (int) itemRow.get("quantidade");

          try {
            com.restaurante.business.gestProdutos.Produto produto = produtoDAO.read(produtoId);
            if (produto != null && quantidade > 0) {
              itens.add(
                  new com.restaurante.business.gestPedidos.ItemPedido(
                      quantidade, "", produto, new ArrayList<>()));
            }
          } catch (Exception ex) {
            // Produto não encontrado
          }
        }
      }

      Pedido pedido = new Pedido(id, pronto, dataHora, pago, itens, total);
      pedidos.add(pedido);
      super.put(pedido.getId(), pedido);
    }

    return pedidos;
  }

  @Override
  public void create(Pedido pedido) throws Exception {
    String sqlPedido = "INSERT INTO pedido (id, data_hora, pronto, pago, metodo_pagamento, total, restaurante_id) VALUES (?, ?, ?, ?, ?, ?, ?)";
    String sqlItem = "INSERT INTO item_pedido (id, pedido_id, produto_id, quantidade, preco_unitario) VALUES (?, ?, ?, ?, ?)";
    String sqlProduto = "INSERT OR IGNORE INTO produto (id, designacao, preco, tipo, disponivel, volume_l, stock) VALUES (?, ?, ?, ?, ?, ?, ?)";

    List<com.restaurante.business.gestProdutos.Menu> menusToInsert = new ArrayList<>();

    try (Connection conn = dbManager.getConnection();
        PreparedStatement pstmtPedido = conn.prepareStatement(sqlPedido);
        PreparedStatement pstmtItem = conn.prepareStatement(sqlItem)) {

      // Inserir pedido
      pstmtPedido.setString(1, pedido.getId());
      pstmtPedido.setString(2, pedido.getDataHora().toString());
      pstmtPedido.setInt(3, pedido.isPronto() ? 1 : 0);
      pstmtPedido.setInt(4, pedido.isPago() ? 1 : 0);
      pstmtPedido.setString(5, null);
      pstmtPedido.setDouble(6, pedido.calculaTotal());
      pstmtPedido.setString(7, null);
      pstmtPedido.executeUpdate();

      // Inserir itens do pedido
      if (pedido.getItems() != null) {
        int idx = 1;
        for (var item : pedido.getItems()) {
          if (item.getProduto() == null)
            continue;
          var prod = item.getProduto();

          // Se for Menu, garantir que existe
          if (prod.getClass().getSimpleName().equals("Menu") || prod.getId().startsWith("MENU-")) {
            try (PreparedStatement pstmtProduto = conn.prepareStatement(sqlProduto)) {
              pstmtProduto.setString(1, prod.getId());
              pstmtProduto.setString(2, prod.getDesignacao());
              pstmtProduto.setDouble(3, prod.getPreco());
              pstmtProduto.setString(4, "MENU");
              pstmtProduto.setInt(5, 1);
              pstmtProduto.setObject(6, null);
              pstmtProduto.setObject(7, null);
              pstmtProduto.executeUpdate();
            }

            try {
              Menu menu = (Menu) prod;
              menusToInsert.add(menu);
            } catch (Exception ignore) {
            }
          }

          pstmtItem.setString(1, pedido.getId() + "-" + idx);
          pstmtItem.setString(2, pedido.getId());
          pstmtItem.setString(3, prod.getId());
          pstmtItem.setInt(4, item.getQuantidade());
          pstmtItem.setDouble(5, prod.getPreco());
          pstmtItem.executeUpdate();
          idx++;
        }
      }
      super.put(pedido.getId(), pedido);
    }
    // Insere menus fora do bloco de conexão principal
    for (Menu menu : menusToInsert) {
      try {
        new com.restaurante.data.dao.MenuDAO().create(menu);
      } catch (Exception ignore) {
      }
    }
  }

  @Override
  public Pedido read(String id) throws Exception {
    String sqlPedido = "SELECT id, data_hora, pronto, pago, metodo_pagamento, total, restaurante_id FROM pedido WHERE id = ?";
    String sqlItens = "SELECT produto_id, quantidade FROM item_pedido WHERE pedido_id = ?";

    // Carregar pedido
    Map<String, Object> pedidoRow = null;
    try (Connection conn = dbManager.getConnection();
        PreparedStatement pstmt = conn.prepareStatement(sqlPedido)) {

      pstmt.setString(1, id);

      try (ResultSet rs = pstmt.executeQuery()) {
        if (rs.next()) {
          pedidoRow = new HashMap<>();
          pedidoRow.put("id", rs.getString("id"));
          pedidoRow.put("data_hora", rs.getString("data_hora"));
          pedidoRow.put("pronto", rs.getInt("pronto"));
          pedidoRow.put("pago", rs.getInt("pago"));
          pedidoRow.put("total", rs.getDouble("total"));
          pedidoRow.put("restaurante_id", rs.getString("restaurante_id"));
        }
      }
    }

    if (pedidoRow == null) {
      return null;
    }

    // Carregar itens
    List<Map<String, Object>> itemRows = new ArrayList<>();
    try (Connection conn = dbManager.getConnection();
        PreparedStatement pstmt = conn.prepareStatement(sqlItens)) {

      pstmt.setString(1, id);

      try (ResultSet rs = pstmt.executeQuery()) {
        while (rs.next()) {
          Map<String, Object> row = new HashMap<>();
          row.put("pedido_id", id);
          row.put("produto_id", rs.getString("produto_id"));
          row.put("quantidade", rs.getInt("quantidade"));
          itemRows.add(row);
        }
      }
    }

    // Construir pedido
    String dataHoraStr = (String) pedidoRow.get("data_hora");
    LocalDateTime dataHora;
    try {
      dataHora = LocalDateTime.parse(dataHoraStr);
    } catch (Exception e) {
      try {
        DateTimeFormatter formatter = DateTimeFormatter.ofPattern("yyyy-MM-dd HH:mm:ss");
        dataHora = LocalDateTime.parse(dataHoraStr, formatter);
      } catch (Exception e2) {
        dataHora = LocalDateTime.now();
      }
    }

    boolean pronto = (int) pedidoRow.get("pronto") == 1;
    boolean pago = (int) pedidoRow.get("pago") == 1;
    double total = (double) pedidoRow.get("total");

    ProdutoDAO produtoDAO = new ProdutoDAO();
    List<com.restaurante.business.gestPedidos.ItemPedido> itens = new ArrayList<>();
    for (Map<String, Object> itemRow : itemRows) {
      if (id.equals(itemRow.get("pedido_id"))) {
        String produtoId = (String) itemRow.get("produto_id");
        int quantidade = (int) itemRow.get("quantidade");

        try {
          com.restaurante.business.gestProdutos.Produto produto = produtoDAO.read(produtoId);
          if (produto != null && quantidade > 0) {
            itens.add(
                new com.restaurante.business.gestPedidos.ItemPedido(
                    quantidade, "", produto, new ArrayList<>()));
          }
        } catch (Exception ex) {
          // Produto não encontrado
        }
      }
    }

    Pedido pedido = new Pedido(id, pronto, dataHora, pago, itens, total);
    super.put(id, pedido);
    return pedido;
  }

  @Override
  public List<Pedido> readAll() throws Exception {
    String sql = "SELECT id, data_hora, pronto, pago, metodo_pagamento, total, restaurante_id FROM pedido";
    String sqlItens = "SELECT pedido_id, produto_id, quantidade FROM item_pedido";
    List<Pedido> pedidos = new ArrayList<>();
    this.clear();

    // Carregar pedidos
    List<Map<String, Object>> pedidoRows = new ArrayList<>();
    try (Connection conn = dbManager.getConnection();
        Statement stmt = conn.createStatement();
        ResultSet rs = stmt.executeQuery(sql)) {

      while (rs.next()) {
        Map<String, Object> row = new HashMap<>();
        row.put("id", rs.getString("id"));
        row.put("data_hora", rs.getString("data_hora"));
        row.put("pronto", rs.getInt("pronto"));
        row.put("pago", rs.getInt("pago"));
        row.put("total", rs.getDouble("total"));
        row.put("restaurante_id", rs.getString("restaurante_id"));
        pedidoRows.add(row);
      }
    }

    // Carregar itens
    List<Map<String, Object>> itemRows = new ArrayList<>();
    try (Connection conn = dbManager.getConnection();
        Statement stmt = conn.createStatement();
        ResultSet rs = stmt.executeQuery(sqlItens)) {

      while (rs.next()) {
        Map<String, Object> row = new HashMap<>();
        row.put("pedido_id", rs.getString("pedido_id"));
        row.put("produto_id", rs.getString("produto_id"));
        row.put("quantidade", rs.getInt("quantidade"));
        itemRows.add(row);
      }
    }

    // Construir pedidos
    ProdutoDAO produtoDAO = new ProdutoDAO();
    for (Map<String, Object> row : pedidoRows) {
      String id = (String) row.get("id");
      String dataHoraStr = (String) row.get("data_hora");
      LocalDateTime dataHora;
      try {
        dataHora = LocalDateTime.parse(dataHoraStr);
      } catch (Exception e) {
        try {
          DateTimeFormatter formatter = DateTimeFormatter.ofPattern("yyyy-MM-dd HH:mm:ss");
          dataHora = LocalDateTime.parse(dataHoraStr, formatter);
        } catch (Exception e2) {
          dataHora = LocalDateTime.now();
        }
      }

      boolean pronto = ((int) row.get("pronto")) == 1;
      boolean pago = ((int) row.get("pago")) == 1;
      double total = (double) row.get("total");

      List<com.restaurante.business.gestPedidos.ItemPedido> itens = new ArrayList<>();
      for (Map<String, Object> itemRow : itemRows) {
        if (id.equals(itemRow.get("pedido_id"))) {
          String produtoId = (String) itemRow.get("produto_id");
          int quantidade = (int) itemRow.get("quantidade");

          try {
            com.restaurante.business.gestProdutos.Produto produto = produtoDAO.read(produtoId);
            if (produto != null && quantidade > 0) {
              itens.add(
                  new com.restaurante.business.gestPedidos.ItemPedido(
                      quantidade, "", produto, new ArrayList<>()));
            }
          } catch (Exception ex) {
            // Produto não encontrado
          }
        }
      }

      Pedido pedido = new Pedido(id, pronto, dataHora, pago, itens, total);
      pedidos.add(pedido);
      super.put(pedido.getId(), pedido);
    }
    return pedidos;
  }

  @Override
  public void update(Pedido pedido) throws Exception {
    String sql = "UPDATE pedido SET pronto = ?, pago = ?, metodo_pagamento = ?, total = ? WHERE id = ?";

    try (Connection conn = dbManager.getConnection();
        PreparedStatement pstmt = conn.prepareStatement(sql)) {

      pstmt.setInt(1, pedido.isPronto() ? 1 : 0);
      pstmt.setInt(2, pedido.isPago() ? 1 : 0);
      pstmt.setString(3, null);
      pstmt.setDouble(4, pedido.calculaTotal());
      pstmt.setString(5, pedido.getId());

      pstmt.executeUpdate();
      super.put(pedido.getId(), pedido);
    }
  }

  @Override
  public void delete(String id) throws Exception {
    String sql = "DELETE FROM pedido WHERE id = ?";

    try (Connection conn = dbManager.getConnection();
        PreparedStatement pstmt = conn.prepareStatement(sql)) {

      pstmt.setString(1, id);
      pstmt.executeUpdate();
      super.remove(id);
    }
  }

  /** Obtém todos os pedidos prontos */
  public List<Pedido> readPedidosProntos() throws Exception {
    String sqlPedidos = "SELECT id, data_hora, pronto, pago, metodo_pagamento, total, restaurante_id FROM pedido WHERE pronto = 1";
    String sqlItens = "SELECT pedido_id, produto_id, quantidade FROM item_pedido";

    List<Pedido> pedidos = new ArrayList<>();

    // Carregar pedidos
    List<Map<String, Object>> pedidoRows = new ArrayList<>();
    try (Connection conn = dbManager.getConnection();
        Statement stmt = conn.createStatement();
        ResultSet rs = stmt.executeQuery(sqlPedidos)) {

      while (rs.next()) {
        Map<String, Object> row = new HashMap<>();
        row.put("id", rs.getString("id"));
        row.put("data_hora", rs.getString("data_hora"));
        row.put("pronto", rs.getInt("pronto"));
        row.put("pago", rs.getInt("pago"));
        row.put("total", rs.getDouble("total"));
        row.put("restaurante_id", rs.getString("restaurante_id"));
        pedidoRows.add(row);
      }
    }

    // Carregar itens
    List<Map<String, Object>> itemRows = new ArrayList<>();
    try (Connection conn = dbManager.getConnection();
        Statement stmt = conn.createStatement();
        ResultSet rs = stmt.executeQuery(sqlItens)) {

      while (rs.next()) {
        Map<String, Object> row = new HashMap<>();
        row.put("pedido_id", rs.getString("pedido_id"));
        row.put("produto_id", rs.getString("produto_id"));
        row.put("quantidade", rs.getInt("quantidade"));
        itemRows.add(row);
      }
    }

    // Construir pedidos
    ProdutoDAO produtoDAO = new ProdutoDAO();
    for (Map<String, Object> row : pedidoRows) {
      String id = (String) row.get("id");
      String dataHoraStr = (String) row.get("data_hora");
      LocalDateTime dataHora;
      try {
        dataHora = LocalDateTime.parse(dataHoraStr);
      } catch (Exception e) {
        try {
          DateTimeFormatter formatter = DateTimeFormatter.ofPattern("yyyy-MM-dd HH:mm:ss");
          dataHora = LocalDateTime.parse(dataHoraStr, formatter);
        } catch (Exception e2) {
          dataHora = LocalDateTime.now();
        }
      }

      boolean pronto = (int) row.get("pronto") == 1;
      boolean pago = (int) row.get("pago") == 1;
      double total = (double) row.get("total");

      List<com.restaurante.business.gestPedidos.ItemPedido> itens = new ArrayList<>();
      for (Map<String, Object> itemRow : itemRows) {
        if (id.equals(itemRow.get("pedido_id"))) {
          String produtoId = (String) itemRow.get("produto_id");
          int quantidade = (int) itemRow.get("quantidade");

          try {
            com.restaurante.business.gestProdutos.Produto produto = produtoDAO.read(produtoId);
            if (produto != null && quantidade > 0) {
              itens.add(
                  new com.restaurante.business.gestPedidos.ItemPedido(
                      quantidade, "", produto, new ArrayList<>()));
            }
          } catch (Exception ex) {
            // Produto não encontrado
          }
        }
      }

      pedidos.add(new Pedido(id, pronto, dataHora, pago, itens, total));
    }
    return pedidos;
  }

  /** Obtém todos os pedidos por pagar */
  public List<Pedido> readPedidosNaoPagos() throws Exception {
    String sqlPedidos = "SELECT id, data_hora, pronto, pago, metodo_pagamento, total, restaurante_id FROM pedido WHERE pago = 0";
    String sqlItens = "SELECT pedido_id, produto_id, quantidade FROM item_pedido";

    List<Pedido> pedidos = new ArrayList<>();

    // Carregar pedidos
    List<Map<String, Object>> pedidoRows = new ArrayList<>();
    try (Connection conn = dbManager.getConnection();
        Statement stmt = conn.createStatement();
        ResultSet rs = stmt.executeQuery(sqlPedidos)) {

      while (rs.next()) {
        Map<String, Object> row = new HashMap<>();
        row.put("id", rs.getString("id"));
        row.put("data_hora", rs.getString("data_hora"));
        row.put("pronto", rs.getInt("pronto"));
        row.put("pago", rs.getInt("pago"));
        row.put("total", rs.getDouble("total"));
        row.put("restaurante_id", rs.getString("restaurante_id"));
        pedidoRows.add(row);
      }
    }

    // Carregar itens
    List<Map<String, Object>> itemRows = new ArrayList<>();
    try (Connection conn = dbManager.getConnection();
        Statement stmt = conn.createStatement();
        ResultSet rs = stmt.executeQuery(sqlItens)) {

      while (rs.next()) {
        Map<String, Object> row = new HashMap<>();
        row.put("pedido_id", rs.getString("pedido_id"));
        row.put("produto_id", rs.getString("produto_id"));
        row.put("quantidade", rs.getInt("quantidade"));
        itemRows.add(row);
      }
    }

    // Construir pedidos
    ProdutoDAO produtoDAO = new ProdutoDAO();
    for (Map<String, Object> row : pedidoRows) {
      String id = (String) row.get("id");
      String dataHoraStr = (String) row.get("data_hora");
      LocalDateTime dataHora;
      try {
        dataHora = LocalDateTime.parse(dataHoraStr);
      } catch (Exception e) {
        try {
          DateTimeFormatter formatter = DateTimeFormatter.ofPattern("yyyy-MM-dd HH:mm:ss");
          dataHora = LocalDateTime.parse(dataHoraStr, formatter);
        } catch (Exception e2) {
          dataHora = LocalDateTime.now();
        }
      }

      boolean pronto = (int) row.get("pronto") == 1;
      boolean pago = (int) row.get("pago") == 1;
      double total = (double) row.get("total");

      List<com.restaurante.business.gestPedidos.ItemPedido> itens = new ArrayList<>();
      for (Map<String, Object> itemRow : itemRows) {
        if (id.equals(itemRow.get("pedido_id"))) {
          String produtoId = (String) itemRow.get("produto_id");
          int quantidade = (int) itemRow.get("quantidade");

          try {
            com.restaurante.business.gestProdutos.Produto produto = produtoDAO.read(produtoId);
            if (produto != null && quantidade > 0) {
              itens.add(
                  new com.restaurante.business.gestPedidos.ItemPedido(
                      quantidade, "", produto, new ArrayList<>()));
            }
          } catch (Exception ex) {
            // Produto não encontrado
          }
        }
      }

      pedidos.add(new Pedido(id, pronto, dataHora, pago, itens, total));
    }
    return pedidos;
  }

  /**
   * Busca todos os pedidos que ainda não têm restaurante_id (não confirmados)
   */
  public List<Pedido> readPedidosNaoConfirmados() throws Exception {
    String sqlPedidos = "SELECT id, data_hora, pronto, pago, metodo_pagamento, total, restaurante_id FROM pedido WHERE restaurante_id IS NULL";
    String sqlItens = "SELECT pedido_id, produto_id, quantidade FROM item_pedido";

    List<Pedido> pedidos = new ArrayList<>();

    // Carregar pedidos
    List<Map<String, Object>> pedidoRows = new ArrayList<>();
    try (Connection conn = dbManager.getConnection();
        Statement stmt = conn.createStatement();
        ResultSet rs = stmt.executeQuery(sqlPedidos)) {

      while (rs.next()) {
        Map<String, Object> row = new HashMap<>();
        row.put("id", rs.getString("id"));
        row.put("data_hora", rs.getString("data_hora"));
        row.put("pronto", rs.getInt("pronto"));
        row.put("pago", rs.getInt("pago"));
        row.put("total", rs.getDouble("total"));
        row.put("restaurante_id", rs.getString("restaurante_id"));
        pedidoRows.add(row);
      }
    }

    // Carregar itens
    List<Map<String, Object>> itemRows = new ArrayList<>();
    try (Connection conn = dbManager.getConnection();
        Statement stmt = conn.createStatement();
        ResultSet rs = stmt.executeQuery(sqlItens)) {

      while (rs.next()) {
        Map<String, Object> row = new HashMap<>();
        row.put("pedido_id", rs.getString("pedido_id"));
        row.put("produto_id", rs.getString("produto_id"));
        row.put("quantidade", rs.getInt("quantidade"));
        itemRows.add(row);
      }
    }

    // Construir pedidos
    ProdutoDAO produtoDAO = new ProdutoDAO();
    for (Map<String, Object> pedidoRow : pedidoRows) {
      String id = (String) pedidoRow.get("id");
      String dataHoraStr = (String) pedidoRow.get("data_hora");

      LocalDateTime dataHora;
      try {
        dataHora = LocalDateTime.parse(dataHoraStr);
      } catch (Exception e) {
        try {
          DateTimeFormatter formatter = DateTimeFormatter.ofPattern("yyyy-MM-dd HH:mm:ss");
          dataHora = LocalDateTime.parse(dataHoraStr, formatter);
        } catch (Exception e2) {
          dataHora = LocalDateTime.now();
        }
      }

      boolean pronto = (int) pedidoRow.get("pronto") == 1;
      boolean pago = (int) pedidoRow.get("pago") == 1;
      double total = (double) pedidoRow.get("total");

      List<com.restaurante.business.gestPedidos.ItemPedido> itens = new ArrayList<>();
      for (Map<String, Object> itemRow : itemRows) {
        if (id.equals(itemRow.get("pedido_id"))) {
          String produtoId = (String) itemRow.get("produto_id");
          int quantidade = (int) itemRow.get("quantidade");

          try {
            com.restaurante.business.gestProdutos.Produto produto = produtoDAO.read(produtoId);
            if (produto != null && quantidade > 0) {
              itens.add(
                  new com.restaurante.business.gestPedidos.ItemPedido(
                      quantidade, "", produto, new ArrayList<>()));
            }
          } catch (Exception ex) {
            // Produto não encontrado
          }
        }
      }

      Pedido pedido = new Pedido(id, pronto, dataHora, pago, itens, total);
      pedidos.add(pedido);
    }

    return pedidos;
  }

  public boolean confirmarPedido(String idPedido, String idRest) throws Exception {
    String sqlUpdate = "UPDATE pedido SET restaurante_id = ? WHERE id = ?";
    String sqlCheckRestaurante = "SELECT 1 FROM restaurante WHERE id = ?";

    try (Connection conn = dbManager.getConnection()) {

      // Validar parâmetros
      if (idRest == null || idPedido == null) {
        return false;
      }

      try (PreparedStatement pstmtCheck = conn.prepareStatement(sqlCheckRestaurante)) {
        pstmtCheck.setString(1, idRest);
        try (ResultSet rs = pstmtCheck.executeQuery()) {
          if (!rs.next()) {
            System.err.println("Erro: Restaurante '" + idRest + "' não existe");
            return false;
          }
        }
      }

      try (PreparedStatement pstmtUpdate = conn.prepareStatement(sqlUpdate)) {
        pstmtUpdate.setString(1, idRest);
        pstmtUpdate.setString(2, idPedido);

        int affectedRows = pstmtUpdate.executeUpdate();

        if (affectedRows == 0) {
          System.err.println("Erro: Pedido '" + idPedido + "' não existe");
        }

        return affectedRows > 0;
      }
    }
  }

}