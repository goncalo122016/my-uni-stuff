package com.restaurante.data.dao;

import java.sql.Connection;
import java.sql.PreparedStatement;
import java.sql.ResultSet;
import java.sql.SQLException;
import java.sql.Statement;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;

import com.restaurante.business.gestProdutos.Bebida;
import com.restaurante.business.gestProdutos.Ingrediente;
import com.restaurante.business.gestProdutos.Prato;
import com.restaurante.business.gestProdutos.Produto;
import com.restaurante.data.DatabaseManager;

/**
 * Data Access Object para Produtos Implementa a interface Map para permitir acesso direto aos dados
 */
public class ProdutoDAO extends HashMap<String, Produto> implements IGenericDAO<Produto> {

  private final DatabaseManager dbManager;

  public ProdutoDAO() {
    this.dbManager = DatabaseManager.getInstance();
  }
  /** Consulta todos os produtos disponíveis diretamente da BD */
  public List<Produto> listarProdutosDisponiveis() throws Exception {
    String sql =
        "SELECT id, designacao, preco, tipo, disponivel, volume_l, stock FROM produto WHERE disponivel = 1";
    List<Produto> produtos = new ArrayList<>();
    try (Connection conn = dbManager.getConnection();
        Statement stmt = conn.createStatement();
        ResultSet rs = stmt.executeQuery(sql)) {
      while (rs.next()) {
        String id = rs.getString("id");
        String designacao = rs.getString("designacao");
        double preco = rs.getDouble("preco");
        String tipo = rs.getString("tipo");
        boolean disponivel = rs.getInt("disponivel") == 1;
        Produto produto;
        if ("PRATO".equals(tipo)) {
          List<Ingrediente> composicao = readComposicaoDoPrato(conn, id);
          Prato prato = new Prato(id, designacao, preco, disponivel, composicao);
          List<Ingrediente> opcoes = readOpcoesDoPrato(conn, id);
          prato.setOpcoes(opcoes);
          produto = prato;
        } else {
          double v = rs.getDouble("volume_l");
          if (rs.wasNull()) v = 0.0;
          float volume = (float) v;
          int stock = rs.getInt("stock");
          if (rs.wasNull()) stock = 0;
          produto = new Bebida(id, designacao, preco, disponivel, volume, stock);
        }
        produtos.add(produto);
      }
    }
    return produtos;
  }


  @Override
  public void create(Produto produto) throws Exception {
    String sql =
        "INSERT INTO produto (id, designacao, preco, tipo, disponivel, volume_l, stock) VALUES (?, ?, ?, ?, ?, ?, ?)";

    try (Connection conn = dbManager.getConnection();
        PreparedStatement pstmt = conn.prepareStatement(sql)) {

      pstmt.setString(1, produto.getId());
      pstmt.setString(2, produto.getDesignacao());
      pstmt.setDouble(3, produto.getPreco());

      String tipo = (produto instanceof Prato) ? "PRATO" : "BEBIDA";
      pstmt.setString(4, tipo);
      pstmt.setInt(5, produto.isDisponivel() ? 1 : 0);
      if (produto instanceof Bebida b) {
        pstmt.setDouble(6, b.getVolumeL());
        pstmt.setInt(7, b.getStock());
      } else {
        pstmt.setNull(6, java.sql.Types.REAL);
        pstmt.setNull(7, java.sql.Types.INTEGER);
      }

      pstmt.executeUpdate();
      // Não guardar em cache, sempre consultar a BD
    }
  }

  @Override
  public Produto read(String id) throws Exception {
    String sql =
        "SELECT id, designacao, preco, tipo, disponivel, volume_l, stock FROM produto WHERE id = ?";
    try (Connection conn = dbManager.getConnection();
        PreparedStatement pstmt = conn.prepareStatement(sql)) {
      pstmt.setString(1, id);
      ResultSet rs = pstmt.executeQuery();
      if (rs.next()) {
        String tipo = rs.getString("tipo");
        String designacao = rs.getString("designacao");
        double preco = rs.getDouble("preco");
        boolean disponivel = rs.getInt("disponivel") == 1;

        // Verifica se é um Menu
        if ("MENU".equals(tipo) || id.startsWith("MENU-") || tipo == null) {
          // Busca dados do Menu na tabela menu
          String menuSql = "SELECT prato_id, bebida_id FROM menu WHERE id = ?";
          try (PreparedStatement menuStmt = conn.prepareStatement(menuSql)) {
            menuStmt.setString(1, id);
            ResultSet menuRs = menuStmt.executeQuery();
            if (menuRs.next()) {
              String pratoId = menuRs.getString("prato_id");
              String bebidaId = menuRs.getString("bebida_id");
              Prato prato = (Prato) this.read(pratoId);
              Bebida bebida = (Bebida) this.read(bebidaId);
              if (prato != null && bebida != null) {
                return new com.restaurante.business.gestProdutos.Menu(
                    id, designacao, prato, bebida);
              }
            }
          }
        }

        Produto produto;
        if ("PRATO".equals(tipo)) {
          List<Ingrediente> composicao = readComposicaoDoPrato(conn, id);
          Prato prato = new Prato(id, designacao, preco, disponivel, composicao);
          List<Ingrediente> opcoes = readOpcoesDoPrato(conn, id);
          prato.setOpcoes(opcoes);
          produto = prato;
        } else if ("BEBIDA".equals(tipo)) {
          float volume = (float) (rs.wasNull() ? 0.0 : rs.getDouble("volume_l"));
          int stock = rs.getInt("stock");
          if (rs.wasNull()) stock = 0;
          produto = new Bebida(id, designacao, preco, disponivel, volume, stock);
        } else {
          // Tipo desconhecido, retorna null
          produto = null;
        }
        return produto;
      }
    }
    return null;
  }

  @Override
  public List<Produto> readAll() throws Exception {
    String sql = "SELECT id, designacao, preco, tipo, disponivel, volume_l, stock FROM produto";
    List<Produto> produtos = new ArrayList<>();
    // Não limpar cache, sempre consultar a BD

    try (Connection conn = dbManager.getConnection();
        Statement stmt = conn.createStatement();
        ResultSet rs = stmt.executeQuery(sql)) {

      while (rs.next()) {
        String id = rs.getString("id");
        String designacao = rs.getString("designacao");
        double preco = rs.getDouble("preco");
        String tipo = rs.getString("tipo");
        boolean disponivel = rs.getInt("disponivel") == 1;

        Produto produto;
        if ("PRATO".equals(tipo)) {
          List<Ingrediente> composicao = readComposicaoDoPrato(conn, id);
          Prato prato = new Prato(id, designacao, preco, disponivel, composicao);
          List<Ingrediente> opcoes = readOpcoesDoPrato(conn, id);
          prato.setOpcoes(opcoes);
          produto = prato;
        } else {
          double v = rs.getDouble("volume_l");
          if (rs.wasNull()) v = 0.0;
          float volume = (float) v;
          int stock = rs.getInt("stock");
          if (rs.wasNull()) stock = 0;
          produto = new Bebida(id, designacao, preco, disponivel, volume, stock);
        }
        produtos.add(produto);
        super.put(produto.getId(), produto); // Sincroniza cache
      }
    }
    return produtos;
  }

  @Override
  public void update(Produto produto) throws Exception {
    String sql =
        "UPDATE produto SET designacao = ?, preco = ?, disponivel = ?, volume_l = ?, stock = ? WHERE id = ?";

    try (Connection conn = dbManager.getConnection();
        PreparedStatement pstmt = conn.prepareStatement(sql)) {

      pstmt.setString(1, produto.getDesignacao());
      pstmt.setDouble(2, produto.getPreco());
      pstmt.setInt(3, produto.isDisponivel() ? 1 : 0);
      if (produto instanceof Bebida b) {
        pstmt.setDouble(4, b.getVolumeL());
        pstmt.setInt(5, b.getStock());
      } else {
        pstmt.setNull(4, java.sql.Types.REAL);
        pstmt.setNull(5, java.sql.Types.INTEGER);
      }
      pstmt.setString(6, produto.getId());

      pstmt.executeUpdate();
      super.put(produto.getId(), produto); // Sincroniza cache
    }
  }

  @Override
  public void delete(String id) throws Exception {
    String sql = "DELETE FROM produto WHERE id = ?";

    try (Connection conn = dbManager.getConnection();
        PreparedStatement pstmt = conn.prepareStatement(sql)) {

      pstmt.setString(1, id);
      pstmt.executeUpdate();
      super.remove(id); // Remove do cache
    }
  }

  /** Obtém todos os pratos */
  public List<Prato> readAllPratos() throws Exception {
    String sql = "SELECT id, designacao, preco, tipo, disponivel FROM produto WHERE tipo = 'PRATO'";
    List<Prato> pratos = new ArrayList<>();

    try (Connection conn = dbManager.getConnection();
        Statement stmt = conn.createStatement();
        ResultSet rs = stmt.executeQuery(sql)) {

      while (rs.next()) {
        String pratoId = rs.getString("id");
        List<Ingrediente> composicao = readComposicaoDoPrato(conn, pratoId);
        Prato prato =
            new Prato(
                pratoId,
                rs.getString("designacao"),
                rs.getDouble("preco"),
                rs.getInt("disponivel") == 1,
                composicao);
        prato.setOpcoes(readOpcoesDoPrato(conn, pratoId));
        pratos.add(prato);
      }
    }
    return pratos;
  }

  /** Obtém todas as bebidas */
  public List<Bebida> readAllBebidas() throws Exception {
    String sql =
        "SELECT id, designacao, preco, tipo, disponivel, volume_l, stock FROM produto WHERE tipo = 'BEBIDA'";
    List<Bebida> bebidas = new ArrayList<>();

    try (Connection conn = dbManager.getConnection();
        Statement stmt = conn.createStatement();
        ResultSet rs = stmt.executeQuery(sql)) {

      while (rs.next()) {
        bebidas.add(
            new Bebida(
                rs.getString("id"),
                rs.getString("designacao"),
                rs.getDouble("preco"),
                rs.getInt("disponivel") == 1,
                (float) rs.getDouble("volume_l"),
                rs.getInt("stock")));
      }
    }
    return bebidas;
  }

  /** Lê a composição (ingredientes) de um prato a partir da tabela prato_ingrediente. */
  private List<Ingrediente> readComposicaoDoPrato(Connection conn, String pratoId)
      throws SQLException {
    String sql =
        "SELECT i.id AS id, i.nome AS nome, i.quantidade AS quantidade FROM prato_ingrediente pi JOIN ingrediente i ON pi.ingrediente_id = i.id WHERE pi.prato_id = ?";
    List<Ingrediente> comps = new ArrayList<>();
    try (PreparedStatement ps = conn.prepareStatement(sql)) {
      ps.setString(1, pratoId);
      try (ResultSet rs = ps.executeQuery()) {
        while (rs.next()) {
          String id = rs.getString("id");
          String nome = rs.getString("nome");
          double qtd = rs.getDouble("quantidade");
          int stock = (int) Math.round(qtd);
          double preco = 1.00; // valor fixo para ingredientes, pode ser ajustado
          comps.add(new Ingrediente(id, stock, nome, preco));
        }
      }
    }
    return comps;
  }

  /** Lê as opções associadas a um prato a partir da tabela prato_opcao. */
  private List<Ingrediente> readOpcoesDoPrato(Connection conn, String pratoId) throws SQLException {
    String sql =
        "SELECT i.id AS id, i.nome AS nome, i.quantidade AS quantidade FROM prato_opcao po JOIN ingrediente i ON po.ingrediente_id = i.id WHERE po.prato_id = ?";
    List<Ingrediente> opcoes = new ArrayList<>();
    try (PreparedStatement ps = conn.prepareStatement(sql)) {
      ps.setString(1, pratoId);
      try (ResultSet rs = ps.executeQuery()) {
        while (rs.next()) {
          String id = rs.getString("id");
          String nome = rs.getString("nome");
          double qtd = rs.getDouble("quantidade");
          int stock = (int) Math.round(qtd);
          double preco = 1.00; // valor fixo para ingredientes, pode ser ajustado
          opcoes.add(new Ingrediente(id, stock, nome, preco));
        }
      }
    }
    return opcoes;
  }
}
