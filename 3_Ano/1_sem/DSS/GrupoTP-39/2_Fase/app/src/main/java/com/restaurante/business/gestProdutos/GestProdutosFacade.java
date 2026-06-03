package com.restaurante.business.gestProdutos;

import com.restaurante.data.dao.ProdutoDAO;
import com.restaurante.data.dao.MenuDAO;
import java.util.List;

public class GestProdutosFacade implements IGestProdutos {
  private final ProdutoDAO produtoDAO;
  private final MenuDAO menuDAO;

  public GestProdutosFacade() {
    this.produtoDAO = new ProdutoDAO();
    this.menuDAO = new MenuDAO();
  }

  public GestProdutosFacade(ProdutoDAO produtoDAO, MenuDAO menuDAO) {
    this.produtoDAO = produtoDAO;
    this.menuDAO = menuDAO;
  }

  @Override
  public List<Produto> listarProdutosDisponiveis() {
    try {
      return produtoDAO.listarProdutosDisponiveis();
    } catch (Exception e) {
      return List.of();
    }
  }

  @Override
  public List<Produto> listarTodosProdutos() {
    try {
      return produtoDAO.readAll();
    } catch (Exception e) {
      return List.of();
    }
  }

  @Override
  public Produto consultarProduto(String idProduto) {
    try {
      return produtoDAO.read(idProduto);
    } catch (Exception e) {
      return null;
    }
  }

  @Override
  public List<Prato> listarPratos() {
    try {
      return produtoDAO.readAllPratos();
    } catch (Exception e) {
      return List.of();
    }
  }

  @Override
  public List<Bebida> listarBebidas() {
    try {
      return produtoDAO.readAllBebidas();
    } catch (Exception e) {
      return List.of();
    }
  }

  @Override
  public List<Menu> listarMenus(List<Prato> pratos, List<Bebida> bebidas) {
    try {
      return menuDAO.readAll(pratos, bebidas);
    } catch (Exception e) {
      return List.of();
    }
  }
}
