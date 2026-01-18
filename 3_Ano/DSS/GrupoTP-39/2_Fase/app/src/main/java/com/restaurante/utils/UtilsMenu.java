package com.restaurante.utils;

import java.util.Scanner;

public class UtilsMenu {
  /**
   * Lê um inteiro do utilizador de forma segura. Se a entrada não for válida, retorna o valor por
   * defeito.
   *
   * @param in Scanner para ler input
   * @param def Valor por defeito
   * @return Inteiro lido ou valor por defeito
   */
  public static int readIntSafe(Scanner in, int def) {
    try {
      return Integer.parseInt(in.nextLine().trim());
    } catch (Exception e) {
      return def;
    }
  }

  /**
   * Lê uma string não vazia do utilizador. Se a entrada for vazia, retorna null.
   *
   * @param in Scanner para ler input
   * @param prompt Mensagem a mostrar
   * @return String lida ou null
   */
  public static String readNonEmptyString(Scanner in, String prompt) {
    System.out.print(prompt);
    String s = in.nextLine().trim();
    return s.isEmpty() ? null : s;
  }

  /** Mostra uma mensagem de opção inválida. */
  public static void opcaoInvalida() {
    System.out.println("Opção inválida.");
  }

  /**
   * Mostra uma mensagem de erro genérica.
   *
   * @param msg Mensagem de erro
   */
  public static void erro(String msg) {
    System.out.println("Erro: " + msg);
  }
}
