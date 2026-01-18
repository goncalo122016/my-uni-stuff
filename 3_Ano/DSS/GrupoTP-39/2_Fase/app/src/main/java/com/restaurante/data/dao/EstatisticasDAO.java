package com.restaurante.data.dao;

import com.restaurante.business.gestEstatisticas.Estatisticas;
import com.restaurante.business.gestEstatisticas.EstatisticasCadeia;
import com.restaurante.business.gestEstatisticas.EstatisticasRestaurante;
import com.restaurante.business.gestEstatisticas.Restaurante;
import com.restaurante.data.DatabaseManager;
import java.sql.Connection;
import java.sql.PreparedStatement;
import java.sql.ResultSet;
import java.sql.Statement;
import java.util.ArrayList;
import java.util.List;
import java.util.UUID;

/** DAO para persistência de Estatísticas */
public class EstatisticasDAO implements IGenericDAO<Estatisticas> {

  private final DatabaseManager dbManager;

  public EstatisticasDAO() {
    this.dbManager = DatabaseManager.getInstance();
  }

  /**
   * Autentica um utilizador pelo username e password na base de dados.
   *
   * @return O cargo do utilizador se autenticado, ou null se falhar.
   */
  public String autentica(String user, String pass) {
    String sql = "SELECT cargo FROM perfil WHERE username = ? AND password = ?";
    try (Connection conn = dbManager.getConnection();
        PreparedStatement pstmt = conn.prepareStatement(sql)) {
      pstmt.setString(1, user);
      pstmt.setString(2, pass);
      try (ResultSet rs = pstmt.executeQuery()) {
        if (rs.next()) {
          return rs.getString("cargo");
        }
      }
    } catch (Exception e) {
      // Log ou tratamento de erro
    }
    return null;
  }

  /** Lista todos os restaurantes da base de dados (id, nome) */
  public List<Restaurante> listarRestaurantes() throws Exception {
    String sql = "SELECT id, nome FROM restaurante";
    List<Restaurante> restaurantes = new ArrayList<>();

    try (Connection conn = dbManager.getConnection();
        PreparedStatement pstmt = conn.prepareStatement(sql);
        ResultSet rs = pstmt.executeQuery()) {

      while (rs.next()) {
        String id = rs.getString("id");
        String nome = rs.getString("nome");
        restaurantes.add(new Restaurante(id, nome));
      }
    }

    return restaurantes;
  }

  @Override
  public void create(Estatisticas estatisticas) throws Exception {
    String sql = "INSERT INTO estatisticas (id, tipo, total_pedidos, total_receita, pedidos_entregues, rating_medio) VALUES (?, ?, ?, ?, ?, ?)";

    try (Connection conn = dbManager.getConnection();
        PreparedStatement pstmt = conn.prepareStatement(sql)) {

      String id = UUID.randomUUID().toString();
      pstmt.setString(1, id);
      pstmt.setString(2, estatisticas.getTipo());
      pstmt.setInt(3, estatisticas.getTotalPedidos());
      pstmt.setDouble(4, estatisticas.getTotalReceita());
      // campos não cobertos pelo modelo actual — gravar zeros por enquanto
      pstmt.setInt(5, 0);
      pstmt.setDouble(6, 0.0);

      pstmt.executeUpdate();
    }
  }

  @Override
  public Estatisticas read(String id) throws Exception {
    String sql = "SELECT id, tipo, total_pedidos, total_receita FROM estatisticas WHERE id = ?";

    try (Connection conn = dbManager.getConnection();
        PreparedStatement pstmt = conn.prepareStatement(sql)) {

      pstmt.setString(1, id);
      ResultSet rs = pstmt.executeQuery();

      if (rs.next()) {
        String tipo = rs.getString("tipo");
        int totalPedidos = rs.getInt("total_pedidos");
        double totalReceita = rs.getDouble("total_receita");

        if ("RESTAURANTE".equals(tipo)) {
          return new EstatisticasRestaurante(totalReceita, totalPedidos);
        } else {
          return new EstatisticasCadeia(totalReceita, totalPedidos);
        }
      }
    }
    return null;
  }

  @Override
  public List<Estatisticas> readAll() throws Exception {
    String sql = "SELECT id, tipo, total_pedidos, total_receita FROM estatisticas";
    List<Estatisticas> list = new ArrayList<>();

    try (Connection conn = dbManager.getConnection();
        Statement stmt = conn.createStatement();
        ResultSet rs = stmt.executeQuery(sql)) {

      while (rs.next()) {
        String tipo = rs.getString("tipo");
        int totalPedidos = rs.getInt("total_pedidos");
        double totalReceita = rs.getDouble("total_receita");

        if ("RESTAURANTE".equals(tipo)) {
          list.add(new EstatisticasRestaurante(totalReceita, totalPedidos));
        } else {
          list.add(new EstatisticasCadeia(totalReceita, totalPedidos));
        }
      }
    }

    return list;
  }

  @Override
  public void update(Estatisticas estatisticas) throws Exception {
    String sql = "UPDATE estatisticas SET total_pedidos = ?, total_receita = ? WHERE tipo = ?";

    try (Connection conn = dbManager.getConnection();
        PreparedStatement pstmt = conn.prepareStatement(sql)) {

      pstmt.setInt(1, estatisticas.getTotalPedidos());
      pstmt.setDouble(2, estatisticas.getTotalReceita());
      pstmt.setString(3, estatisticas.getTipo());

      pstmt.executeUpdate();
    }
  }

  @Override
  public void delete(String id) throws Exception {
    String sql = "DELETE FROM estatisticas WHERE id = ?";

    try (Connection conn = dbManager.getConnection();
        PreparedStatement pstmt = conn.prepareStatement(sql)) {

      pstmt.setString(1, id);
      pstmt.executeUpdate();
    }
  }
}
