package com.restaurante.business.gestEstatisticas;

public interface IGestEstatisticas {

  String login(String username, String password);

  EstatisticasCadeia calcularEstatisticasCadeia();

  EstatisticasRestaurante calcularEstatisticasRestaurante(String idRestaurante);

  String getRestauranteIdPorUsername(String username);
}
