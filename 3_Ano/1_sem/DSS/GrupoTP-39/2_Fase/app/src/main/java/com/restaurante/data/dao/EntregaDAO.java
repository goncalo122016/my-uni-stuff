package com.restaurante.data.dao;

import com.restaurante.business.gestEntregas.Entrega;
import com.restaurante.business.gestEntregas.EstadoEntrega;
import com.restaurante.business.gestEntregas.TipoEntrega;
import com.restaurante.business.gestPedidos.Pedido;
import com.restaurante.data.DatabaseManager;
import java.sql.Connection;
import java.sql.PreparedStatement;
import java.sql.ResultSet;
import java.sql.SQLException;
import java.sql.Statement;
import java.time.LocalDateTime;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;

/**
 * Data Access Object para Entregas Implementa a interface Map para permitir acesso direto aos dados
 */
public class EntregaDAO extends HashMap<String, Entrega> implements IGenericDAO<Entrega> {
  private final DatabaseManager dbManager;

  public EntregaDAO() {
    this.dbManager = DatabaseManager.getInstance();
  }

  @Override
  public void create(Entrega entrega) throws Exception {
    String sql =
        "INSERT INTO entrega (id, pedido_id, tipo_entrega, estado, endereco, data_criacao) VALUES (?, ?, ?, ?, ?, ?)";

    try (Connection conn = dbManager.getConnection();
        PreparedStatement pstmt = conn.prepareStatement(sql)) {

      pstmt.setString(1, entrega.getId());
      pstmt.setString(2, entrega.getPedido().getId());
      pstmt.setString(3, entrega.getTipo().toString());
      pstmt.setString(4, entrega.getEstado().toString());
      pstmt.setString(5, entrega.getEndereco());
      pstmt.setString(6, LocalDateTime.now().toString());

      pstmt.executeUpdate();
      super.put(entrega.getId(), entrega); // Sincroniza cache
    }
  }

  @Override
  public Entrega read(String id) throws Exception {
    String sql =
        "SELECT id, pedido_id, tipo_entrega, estado, endereco, data_criacao, data_entrega FROM entrega WHERE id = ?";

    try (Connection conn = dbManager.getConnection();
        PreparedStatement pstmt = conn.prepareStatement(sql)) {

      pstmt.setString(1, id);
      ResultSet rs = pstmt.executeQuery();

      if (rs.next()) {
        Entrega entrega = mapResultSetToEntrega(rs);
        super.put(id, entrega); // Sincroniza cache
        return entrega;
      }
    }
    return null;
  }

  @Override
  public List<Entrega> readAll() throws Exception {
    String sql =
        "SELECT id, pedido_id, tipo_entrega, estado, endereco, data_criacao, data_entrega FROM entrega";
    List<Entrega> entregas = new ArrayList<>();
    this.clear(); // Limpa cache antes de recarregar

    try (Connection conn = dbManager.getConnection();
        Statement stmt = conn.createStatement();
        ResultSet rs = stmt.executeQuery(sql)) {

      while (rs.next()) {
        Entrega entrega = mapResultSetToEntrega(rs);
        entregas.add(entrega);
        super.put(entrega.getId(), entrega); // Sincroniza cache
      }
    }
    return entregas;
  }

  @Override
  public void update(Entrega entrega) throws Exception {
    String sql = "UPDATE entrega SET estado = ?, data_entrega = ? WHERE id = ?";

    try (Connection conn = dbManager.getConnection();
        PreparedStatement pstmt = conn.prepareStatement(sql)) {

      pstmt.setString(1, entrega.getEstado().toString());
      if (entrega.getDataEntrega() != null) {
        pstmt.setString(2, entrega.getDataEntrega().toString());
      } else {
        pstmt.setNull(2, java.sql.Types.VARCHAR);
      }
      pstmt.setString(3, entrega.getId());

      pstmt.executeUpdate();
      super.put(entrega.getId(), entrega); // Sincroniza cache
    }
  }

  @Override
  public void delete(String id) throws Exception {
    String sql = "DELETE FROM entrega WHERE id = ?";

    try (Connection conn = dbManager.getConnection();
        PreparedStatement pstmt = conn.prepareStatement(sql)) {

      pstmt.setString(1, id);
      pstmt.executeUpdate();
      super.remove(id); // Remove do cache
    }
  }

  /** Obtém todas as entregas pendentes */
  public List<Entrega> readEntregasPendentes() throws Exception {
    // Estado na enum é 'ESPERA' para entregas pendentes
    String sql =
        "SELECT id, pedido_id, tipo_entrega, estado, endereco, data_criacao, data_entrega FROM entrega WHERE estado = 'ESPERA'";
    List<Entrega> entregas = new ArrayList<>();

    try (Connection conn = dbManager.getConnection();
        Statement stmt = conn.createStatement();
        ResultSet rs = stmt.executeQuery(sql)) {

      while (rs.next()) {
        entregas.add(mapResultSetToEntrega(rs));
      }
    }
    return entregas;
  }

  /** Obtém todas as entregas de um tipo */
  public List<Entrega> readEntregasPorTipo(TipoEntrega tipo) throws Exception {
    String sql =
        "SELECT id, pedido_id, tipo_entrega, estado, endereco, data_criacao, data_entrega FROM entrega WHERE tipo_entrega = ?";
    List<Entrega> entregas = new ArrayList<>();

    try (Connection conn = dbManager.getConnection();
        PreparedStatement pstmt = conn.prepareStatement(sql)) {

      pstmt.setString(1, tipo.toString());
      ResultSet rs = pstmt.executeQuery();

      while (rs.next()) {
        entregas.add(mapResultSetToEntrega(rs));
      }
    }
    return entregas;
  }

  public List<Entrega> readEntregasPorRestaurante(String idRestaurante) throws Exception {
    String sql =
        "SELECT e.id, e.pedido_id, e.tipo_entrega, e.estado, e.endereco, e.data_criacao, e.data_entrega "
            +
            "FROM entrega e "
            +
            "JOIN pedido p ON e.pedido_id = p.id "
            +
            "WHERE p.restaurante_id = ?";
    List<Entrega> entregas = new ArrayList<>();

    try (Connection conn = dbManager.getConnection();
        PreparedStatement pstmt = conn.prepareStatement(sql)) {

      pstmt.setString(1, idRestaurante);
      ResultSet rs = pstmt.executeQuery();

      while (rs.next()) {
        entregas.add(mapResultSetToEntrega(rs));
      }
    }
    return entregas;
  }

  private Entrega mapResultSetToEntrega(ResultSet rs) throws SQLException {
    String id = rs.getString("id");
    String pedidoId = rs.getString("pedido_id");
    TipoEntrega tipo = TipoEntrega.valueOf(rs.getString("tipo_entrega"));
    EstadoEntrega estado = EstadoEntrega.valueOf(rs.getString("estado"));
    String endereco = rs.getString("endereco");
    String dataCriacaoStr = rs.getString("data_criacao");
    LocalDateTime dataCriacao;
    try {
      dataCriacao = LocalDateTime.parse(dataCriacaoStr);
    } catch (Exception e) {
      // Tenta formato 'yyyy-MM-dd HH:mm:ss'
      java.time.format.DateTimeFormatter fmt =
          java.time.format.DateTimeFormatter.ofPattern("yyyy-MM-dd HH:mm:ss");
      dataCriacao = LocalDateTime.parse(dataCriacaoStr, fmt);
    }
    String dataEntregaStr = rs.getString("data_entrega");
    LocalDateTime dataEntrega = null;
    if (dataEntregaStr != null) {
      try {
        dataEntrega = LocalDateTime.parse(dataEntregaStr);
      } catch (Exception e) {
        java.time.format.DateTimeFormatter fmt =
            java.time.format.DateTimeFormatter.ofPattern("yyyy-MM-dd HH:mm:ss");
        dataEntrega = LocalDateTime.parse(dataEntregaStr, fmt);
      }
    }

    // Cria uma instância de Pedido com ID para representar a relação
    Pedido pedido = new Pedido(pedidoId, false, LocalDateTime.now(), false, new ArrayList<>());

    Entrega entrega = new Entrega(id, dataCriacao, pedido, tipo, estado);
    if (dataEntrega != null) {
      entrega.setDataEntrega(dataEntrega);
    }
    entrega.setEndereco(endereco);
    return entrega;
  }
}
