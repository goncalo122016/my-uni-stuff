package com.restaurante.data.dao;

import com.restaurante.business.gestProdutos.Bebida;
import com.restaurante.business.gestProdutos.Menu;
import com.restaurante.business.gestProdutos.Prato;
import com.restaurante.data.DatabaseManager;
import java.sql.Connection;
import java.sql.PreparedStatement;
import java.sql.ResultSet;
import java.sql.SQLException;
import java.sql.Statement;
import java.util.ArrayList;
import java.util.List;

public class MenuDAO {
  private final DatabaseManager dbManager;

  public MenuDAO() {
    this.dbManager = DatabaseManager.getInstance();
  }

  public void create(Menu menu) throws Exception {
    String sql =
        "INSERT INTO menu (id, designacao, prato_id, bebida_id, preco) VALUES (?, ?, ?, ?, ?)";
    try (Connection conn = dbManager.getConnection();
        PreparedStatement pstmt = conn.prepareStatement(sql)) {
      pstmt.setString(1, menu.getId());
      pstmt.setString(2, menu.getDesignacao());
      pstmt.setString(3, menu.getPrato().getId());
      pstmt.setString(4, menu.getBebida().getId());
      pstmt.setDouble(5, menu.getPreco());
      pstmt.executeUpdate();
    }
  }

  public Menu read(String id, Prato prato, Bebida bebida) throws Exception {
    String sql = "SELECT id, designacao, prato_id, bebida_id, preco FROM menu WHERE id = ?";
    try (Connection conn = dbManager.getConnection();
        PreparedStatement pstmt = conn.prepareStatement(sql)) {
      pstmt.setString(1, id);
      ResultSet rs = pstmt.executeQuery();
      if (rs.next()) {
        return new Menu(rs.getString("id"), rs.getString("designacao"), prato, bebida);
      }
    }
    return null;
  }

  public List<Menu> readAll(List<Prato> pratos, List<Bebida> bebidas) throws Exception {
    String sql = "SELECT id, designacao, prato_id, bebida_id, preco FROM menu";
    List<Menu> menus = new ArrayList<>();
    try (Connection conn = dbManager.getConnection();
        Statement stmt = conn.createStatement();
        ResultSet rs = stmt.executeQuery(sql)) {
      while (rs.next()) {
        final String pratoId;
        final String bebidaId;
        try {
          pratoId = rs.getString("prato_id");
        } catch (SQLException e) {
          continue;
        }
        try {
          bebidaId = rs.getString("bebida_id");
        } catch (SQLException e) {
          continue;
        }
        Prato prato =
            pratos.stream().filter(p -> p.getId().equals(pratoId)).findFirst().orElse(null);
        Bebida bebida =
            bebidas.stream().filter(b -> b.getId().equals(bebidaId)).findFirst().orElse(null);
        if (prato != null && bebida != null) {
          menus.add(new Menu(rs.getString("id"), rs.getString("designacao"), prato, bebida));
        }
      }
    }
    return menus;
  }
}
