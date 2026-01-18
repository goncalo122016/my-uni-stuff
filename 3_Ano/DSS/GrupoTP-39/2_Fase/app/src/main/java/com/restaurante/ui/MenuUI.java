package com.restaurante.ui;

import com.restaurante.business.IRestauranteLN;
import com.restaurante.business.RestauranteLNFacade;
import java.util.Scanner;

public class MenuUI {

  private final Scanner in;
  private final IRestauranteLN ln;

  public MenuUI() {
    this.in = new Scanner(System.in);
    this.ln = new RestauranteLNFacade();
  }

  public void run() {
    while (true) {
      System.out.println("\n=== Restaurante ===");
      System.out.println("1) Cliente");
      System.out.println("2) Funcionário");
      System.out.println("3) Chefe");
      System.out.println("4) COO");
      System.out.println("0) Sair");
      System.out.print("> ");

      String opt = in.nextLine().trim();
      switch (opt) {
        case "1" -> new MenuCliente(in, ln).run();
        case "2" -> new MenuFuncionario(in, ln).run();
        case "3" -> new MenuChefe(in, ln).run();
        case "4" -> new MenuCOO(in, ln).run();
        case "0" -> {
          System.out.println("Até breve!");
          return;
        }
        default -> System.out.println("Opção inválida.");
      }
    }
  }
}
