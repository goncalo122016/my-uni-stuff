package com.restaurante.business.gestEntregas;

import com.restaurante.business.gestPedidos.Pedido;
import java.time.LocalDateTime;

public class Entrega {

  private String id;
  private LocalDateTime dataHora; // data de criacao
  private LocalDateTime dataEntrega; // data em que a entrega foi concluida
  private Pedido pedido;
  private TipoEntrega tipo;
  private EstadoEntrega estado;
  private String endereco;

  public Entrega(
      String id, LocalDateTime dataHora, Pedido pedido, TipoEntrega tipo, EstadoEntrega estado) {
    this.id = id;
    this.dataHora = dataHora;
    this.pedido = pedido;
    this.tipo = tipo;
    this.estado = estado;
  }

  // Conveniencia: construtor usado pelo DAO quando apenas temos id, pedido e tipo
  public Entrega(String id, Pedido pedido, TipoEntrega tipo) {
    this.id = id;
    this.dataHora = LocalDateTime.now();
    this.pedido = pedido;
    this.tipo = tipo;
    this.estado = EstadoEntrega.PENDENTE;
  }

  public boolean verificaEntrega() {
    return this.estado == EstadoEntrega.ENTREGUE && this.dataEntrega != null;
  }

  public String getId() {
    return id;
  }

  public LocalDateTime getDataHora() {
    return dataHora;
  }

  public LocalDateTime getDataEntrega() {
    return dataEntrega;
  }

  public void setDataEntrega(LocalDateTime dataEntrega) {
    this.dataEntrega = dataEntrega;
  }

  public Pedido getPedido() {
    return pedido;
  }

  public TipoEntrega getTipo() {
    return tipo;
  }

  public EstadoEntrega getEstado() {
    return estado;
  }

  public void setEstado(EstadoEntrega estado) {
    this.estado = estado;
  }

  public String getEndereco() {
    return endereco;
  }

  public void setEndereco(String endereco) {
    this.endereco = endereco;
  }
}
