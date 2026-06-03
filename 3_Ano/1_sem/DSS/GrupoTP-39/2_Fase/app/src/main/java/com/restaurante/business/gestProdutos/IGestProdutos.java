package com.restaurante.business.gestProdutos;

import java.util.List;

public interface IGestProdutos {

  List<Produto> listarProdutosDisponiveis();

  List<Produto> listarTodosProdutos();

  Produto consultarProduto(String idProduto);

  List<Prato> listarPratos();

  List<Bebida> listarBebidas();

  List<Menu> listarMenus(List<Prato> pratos, List<Bebida> bebidas);
}
