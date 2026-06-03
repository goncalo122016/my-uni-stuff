package com.restaurante.business.gestEntregas;

import com.restaurante.business.gestPedidos.Pedido;
import java.util.List;

public interface IGestEntregas {

  boolean confirmarEntrega(String idEntrega);

  void registarEntrega(Pedido p);

  List<Entrega> listarEntregas();

  List<Entrega> listarEntregasPorRestaurante(String idRestaurante);

  List<Entrega> listarEntregasPendentes();

  Entrega consultarEntrega(String idEntrega);

  boolean recolherEntregaPorPedido(String idPedido);
}
