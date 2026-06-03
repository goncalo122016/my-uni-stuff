package com.restaurante.data.dao;

import com.restaurante.business.gestEstatisticas.Cargo;
import com.restaurante.business.gestEstatisticas.Perfil;
import com.restaurante.data.DatabaseManager;
import java.sql.Connection;
import java.sql.PreparedStatement;
import java.sql.ResultSet;

public class PerfilDAO {
  private final DatabaseManager dbManager;

  public PerfilDAO() {
    this.dbManager = DatabaseManager.getInstance();
  }

  /**
   * Busca o restaurante_id associado a um username
   */
  public String getRestauranteIdPorUsername(String username) throws Exception {
    String sql = "SELECT restaurante_id FROM perfil WHERE username = ?";
    
    try (Connection conn = dbManager.getConnection();
         PreparedStatement pstmt = conn.prepareStatement(sql)) {
      
      pstmt.setString(1, username);
      
      try (ResultSet rs = pstmt.executeQuery()) {
        if (rs.next()) {
          return rs.getString("restaurante_id");
        }
      }
    }
    return null;
  }

  /**
   * Busca um perfil completo por username
   */
  public Perfil getPerfilPorUsername(String username) throws Exception {
    String sql = "SELECT p.id, p.nome, p.username, c.nome as cargo_nome, p.restaurante_id " +
                 "FROM perfil p " +
                 "INNER JOIN cargo c ON p.cargo_id = c.id " +
                 "WHERE p.username = ?";
    
    try (Connection conn = dbManager.getConnection();
         PreparedStatement pstmt = conn.prepareStatement(sql)) {
      
      pstmt.setString(1, username);
      
      try (ResultSet rs = pstmt.executeQuery()) {
        if (rs.next()) {
          String id = rs.getString("id");
          String nome = rs.getString("nome");
          String cargoNome = rs.getString("cargo_nome");
          String idRestaurante = rs.getString("restaurante_id");
          
          Cargo cargo = Cargo.valueOf(cargoNome.toUpperCase());
          
          return new Perfil(id, nome, cargo, idRestaurante);
        }
      }
    }
    return null;
  }
}
