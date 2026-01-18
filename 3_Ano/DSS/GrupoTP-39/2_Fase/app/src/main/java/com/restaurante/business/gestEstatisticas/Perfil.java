package com.restaurante.business.gestEstatisticas;

public class Perfil {
  private String id;
  private String nome;
  private Cargo cargo;
  private String idRestaurante;

  public Perfil(String id, String nome, Cargo cargo, String idRestaurante) {
    this.id = id;
    this.nome = nome;
    this.cargo = cargo;
    this.idRestaurante = idRestaurante;
  }

  public Perfil(String nome, Cargo cargo, String idRestaurante) {
    this.nome = nome;
    this.cargo = cargo;
    this.idRestaurante = idRestaurante;
  }

  public Perfil(Cargo cargo, String nome) {
    this.cargo = cargo;
    this.nome = nome;
  }

  // Getters
  public String getId() {
    return id;
  }

  public String getNome() {
    return nome;
  }

  public Cargo getCargo() {
    return cargo;
  }

  public String getIdRestaurante() {
    return idRestaurante;
  }

  // Setters
  public void setId(String id) {
    this.id = id;
  }

  public void setNome(String nome) {
    this.nome = nome;
  }

  public void setCargo(Cargo cargo) {
    this.cargo = cargo;
  }

  public void setIdRestaurante(String idRestaurante) {
    this.idRestaurante = idRestaurante;
  }

  // Métodos utilitários
  public boolean isCOO() {
    return cargo == Cargo.COO;
  }

  public boolean isChefe() {
    return cargo == Cargo.CHEFE;
  }

  public boolean isFuncionario() {
    return cargo == Cargo.FUNCIONARIO;
  }

  @Override
  public String toString() {
    return "Perfil{" +
        "id='" + id + '\'' +
        ", nome='" + nome + '\'' +
        ", cargo=" + cargo +
        ", idRestaurante='" + idRestaurante + '\'' +
        '}';
  }
}
