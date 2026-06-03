package com.restaurante.data;

import java.io.BufferedReader;
import java.io.File;
import java.io.FileReader;
import java.io.IOException;
import java.sql.Connection;
import java.sql.DriverManager;
import java.sql.SQLException;
import java.sql.Statement;

/** Gerenciador de conexões e inicialização da base de dados SQLite */
public class DatabaseManager {
  private static final String DATABASE_URL = "jdbc:sqlite:restaurante.db";
  private static final String SCHEMA_FILE = "../database/schema.sql";
  private static final String DATA_FILE = "../database/dados_teste.sql";

  private static DatabaseManager instance;
  private Connection connection;

  /** Padrão Singleton para garantir apenas uma instância */
  public static synchronized DatabaseManager getInstance() {
    if (instance == null) {
      instance = new DatabaseManager();
    }
    return instance;
  }

  /** Construtor privado (Singleton) */
  private DatabaseManager() {
    try {
      Class.forName("org.sqlite.JDBC");
    } catch (ClassNotFoundException e) {
      System.err.println("Erro ao carregar driver SQLite: " + e.getMessage());
    }
  }

  /** Obtém uma conexão com a base de dados */
  public Connection getConnection() throws SQLException {
    if (connection == null || connection.isClosed()) {
      connection = DriverManager.getConnection(DATABASE_URL);
      connection.setAutoCommit(true);
    }
    return connection;
  }

  /** Inicializa a base de dados (elimina a anterior, cria tabelas e insere dados de teste) */
  public void initializeDatabase() {
    try {
      System.out.println("Inicializando base de dados...");

      // Eliminar base de dados anterior
      resetDatabase();

      // Executar schema
      executeSQLScript(SCHEMA_FILE);
      System.out.println("✓ Schema criado com sucesso");

      // Executar dados de teste
      executeSQLScript(DATA_FILE);
      System.out.println("✓ Dados de teste carregados");

      System.out.println("✓ Base de dados inicializada com sucesso!");
    } catch (IOException | SQLException e) {
      System.err.println("Erro ao inicializar base de dados: " + e.getMessage());
      e.printStackTrace();
    }
  }

  /** Elimina o ficheiro da base de dados para começar do zero */
  private void resetDatabase() {
    try {
      // Fecha a conexão atual se existir
      if (connection != null && !connection.isClosed()) {
        connection.close();
        connection = null;
      }

      // Elimina o ficheiro da base de dados
      File dbFile = new File("restaurante.db");
      if (dbFile.exists()) {
        if (dbFile.delete()) {
          System.out.println("✓ Base de dados anterior eliminada");
        } else {
          System.out.println("⚠ Não foi possível eliminar a base de dados anterior");
        }
      }
    } catch (SQLException e) {
      System.err.println("Erro ao fechar conexão: " + e.getMessage());
    }
  }

  /** Executa um ficheiro SQL */
  private void executeSQLScript(String filePath) throws IOException, SQLException {
    StringBuilder sqlScript = new StringBuilder();

    try (BufferedReader reader = new BufferedReader(new FileReader(filePath))) {
      String line;
      while ((line = reader.readLine()) != null) {
        // Ignora comentários e linhas vazias
        if (!line.trim().startsWith("--") && !line.trim().isEmpty()) {
          sqlScript.append(line).append("\n");
        }
      }
    }

    // Divide os comandos SQL por ponto e vírgula
    String[] commands = sqlScript.toString().split(";");

    try (Connection conn = getConnection();
        Statement stmt = conn.createStatement()) {

      for (String command : commands) {
        String trimmedCommand = command.trim();
        if (!trimmedCommand.isEmpty()) {
          try {
            stmt.execute(trimmedCommand);
          } catch (SQLException e) {
            String msg = e.getMessage();
            // Ignorar avisos de PK duplicada, tabela existente ou coluna duplicada
            // silenciosamente
            if (msg != null
                && (msg.contains("already exists")
                    || msg.contains("PRIMARY KEY")
                    || msg.contains("UNIQUE constraint failed")
                    || msg.contains("duplicate column name"))) {
              // silencioso
            } else {
              System.out.println("Aviso SQL: " + msg);
            }
          }
        }
      }
    }
  }

  /** Fecha a conexão com a base de dados */
  public void closeConnection() {
    try {
      if (connection != null && !connection.isClosed()) {
        connection.close();
        System.out.println("Conexão com a base de dados fechada");
      }
    } catch (SQLException e) {
      System.err.println("Erro ao fechar conexão: " + e.getMessage());
    }
  }

  /** Limpa a base de dados (remove todas as tabelas) - Útil para testes */
  public void clearDatabase() {
    try (Connection conn = getConnection();
        Statement stmt = conn.createStatement()) {

      // Desabilita chaves estrangeiras temporariamente
      stmt.execute("PRAGMA foreign_keys = OFF");

      // Apaga todas as tabelas
      stmt.execute("DELETE FROM prato_ingrediente");
      stmt.execute("DELETE FROM item_pedido");
      stmt.execute("DELETE FROM entrega");
      stmt.execute("DELETE FROM pedido");
      stmt.execute("DELETE FROM produto");
      stmt.execute("DELETE FROM ingrediente");
      stmt.execute("DELETE FROM perfil");
      stmt.execute("DELETE FROM cargo");
      stmt.execute("DELETE FROM estatisticas");

      // Reabilita chaves estrangeiras
      stmt.execute("PRAGMA foreign_keys = ON");

      System.out.println("Base de dados limpa com sucesso");
    } catch (SQLException e) {
      System.err.println("Erro ao limpar base de dados: " + e.getMessage());
    }
  }

  /** Executa uma query SQL */
  public void executeQuery(String query) throws SQLException {
    try (Connection conn = getConnection();
        Statement stmt = conn.createStatement()) {
      stmt.execute(query);
    }
  }
}
