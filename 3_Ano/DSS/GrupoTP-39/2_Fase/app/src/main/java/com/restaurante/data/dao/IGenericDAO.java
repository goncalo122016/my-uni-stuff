package com.restaurante.data.dao;

import java.util.List;

/** Interface genérica para Data Access Objects */
public interface IGenericDAO<T> {
  void create(T entity) throws Exception;

  T read(String id) throws Exception;

  List<T> readAll() throws Exception;

  void update(T entity) throws Exception;

  void delete(String id) throws Exception;
}
