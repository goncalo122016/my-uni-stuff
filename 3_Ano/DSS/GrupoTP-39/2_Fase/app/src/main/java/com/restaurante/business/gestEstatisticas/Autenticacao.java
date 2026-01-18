package com.restaurante.business.gestEstatisticas;

import com.restaurante.data.dao.EstatisticasDAO;

public class Autenticacao {
  private final EstatisticasDAO estatisticasDAO;

  public Autenticacao(EstatisticasDAO dao) {
    this.estatisticasDAO = dao;
  }

  public String autenticar(String user, String pass) {
    return estatisticasDAO.autentica(user, pass);
  }
}
