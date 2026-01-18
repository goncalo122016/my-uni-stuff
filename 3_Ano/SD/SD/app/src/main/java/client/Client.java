package client;

import java.io.BufferedReader;
import java.io.InputStreamReader;

public class Client {
    public static void main(String[] args) {
        String host = "localhost";
        int port = 12345;

        try {
            System.out.println("\n╔════════════════════════════════════╗");
            System.out.println("║   SISTEMA DISTRIBUÍDO DE EVENTOS   ║");
            System.out.println("╚════════════════════════════════════╝\n");

            showMenu(host, port);

        } catch (Exception e) {
            System.err.println("Erro: " + e.getMessage());
        }
    }

    private static void showMenu(String host, int port) throws Exception {
        BufferedReader reader = new BufferedReader(new InputStreamReader(System.in));
        boolean running = true;
        ClientStub stub = null;
        boolean authenticated = false;

        while (running) {
            System.out.println("\n┌────────────────────────────────────┐");
            System.out.println("│          MENU PRINCIPAL            │");
            System.out.println("├────────────────────────────────────┤");
            if (!authenticated) {
                System.out.println("│  1. Sign In (Criar conta)          │");
                System.out.println("│  2. Log In  (Entrar na conta)      │");
                System.out.println("│  3. Exit (Sair)                    │");
            } else {
                System.out.println("│  1. Menu de Operações              │");
                System.out.println("│  2. Exit (Sair)                    │");
            }
            System.out.println("└────────────────────────────────────┘");
            System.out.print("\nEscolha uma opção: ");

            String choice = reader.readLine();

            if (choice == null || choice.trim().isEmpty()) {
                System.out.println("Opção inválida. Tenta novamente.");
                continue;
            }

            switch (choice.trim()) {
                case "1":
                    if (!authenticated) {
                        if (stub == null) {
                            stub = new ClientStub(host, port);
                        }
                        signIn(stub, reader);
                    } else {
                        operationsMenu(stub, reader);
                    }
                    break;
                case "2":
                    if (!authenticated) {
                        if (stub == null) {
                            stub = new ClientStub(host, port);
                        }
                        authenticated = logIn(stub, reader);
                    } else {
                        System.out.println("\nAté à próxima!");
                        if (stub != null)
                            stub.close();
                        running = false;
                    }
                    break;
                case "3":
                    if (!authenticated) {
                        System.out.println("\nAté à próxima!");
                        if (stub != null)
                            stub.close();
                        running = false;
                    } else {
                        System.out.println("Opção inválida.");
                    }
                    break;
                default:
                    System.out.println("Opção inválida.");
            }
        }
    }

    private static void signIn(ClientStub stub, BufferedReader reader) throws Exception {
        System.out.println("\n▸ Criar nova conta");

        System.out.print("  Username: ");
        String username = reader.readLine();

        System.out.print("  Password: ");
        String password = reader.readLine();

        boolean success = stub.signIn(username, password);

        if (success) {
            System.out.println("\n✓ Conta criada com sucesso!");
        } else {
            System.out.println("\n✗ Erro: Username já existe.");
        }
    }

    private static boolean logIn(ClientStub stub, BufferedReader reader) throws Exception {
        System.out.println("\n▸ Entrar na conta");

        System.out.print("  Username: ");
        String username = reader.readLine();

        System.out.print("  Password: ");
        String password = reader.readLine();

        boolean success = stub.logIn(username, password);

        if (success) {
            System.out.println("\n✓ Login efetuado com sucesso!");
            return true;
        } else {
            System.out.println("\n✗ Credenciais inválidas.");
            return false;
        }
    }

    private static void operationsMenu(ClientStub stub, BufferedReader reader) throws Exception {
        boolean inMenu = true;

        while (inMenu) {
            System.out.println("\n┌────────────────────────────────────┐");
            System.out.println("│       MENU DE OPERAÇÕES            │");
            System.out.println("├────────────────────────────────────┤");
            System.out.println("│  1. Inserir evento                 │");
            System.out.println("│  2. Filtrar eventos por data       │");
            System.out.println("│  3. Notificações                   │");
            System.out.println("│  0. Voltar                         │");
            System.out.println("└────────────────────────────────────┘");
            System.out.print("\nEscolha uma opção: ");

            String choice = reader.readLine();

            if (choice == null || choice.trim().isEmpty()) {
                System.out.println("Opção inválida.");
                continue;
            }

            switch (choice.trim()) {
                case "1":
                    insertEvent(stub, reader);
                    break;
                case "2":
                    filterEvents(stub, reader);
                    break;
                case "3":
                    notificationMenu(stub, reader);
                    break;
                case "0":
                    inMenu = false;
                    break;
                default:
                    System.out.println("Opção inválida.");
            }
        }
    }

    private static void insertEvent(ClientStub stub, BufferedReader reader) throws Exception {
        System.out.println("\n▸ Inserir novo evento");

        System.out.print("  Produto: ");
        String product = reader.readLine();

        System.out.print("  Quantidade: ");
        double quantity = Double.parseDouble(reader.readLine());

        System.out.print("  Preço: ");
        double price = Double.parseDouble(reader.readLine());

        boolean success = stub.insertEvent(product, quantity, price);

        if (success) {
            System.out.println("\n✓ Evento inserido com sucesso!");
        } else {
            System.out.println("\n✗ Erro ao inserir evento.");
        }
    }

    private static void filterEvents(ClientStub stub, BufferedReader reader) throws Exception {
        System.out.println("\n▸ Filtrar eventos por data");
        System.out.print("  Data (YYYY-MM-DD): ");
        String date = reader.readLine();

        ClientStub.FilterResult result = stub.filterEventsByDate(date);
        result.print();
    }

    private static void notificationMenu(ClientStub stub, BufferedReader reader) throws Exception {
        System.out.println("\n┌────────────────────────────────────┐");
        System.out.println("│        NOTIFICAÇÕES                │");
        System.out.println("├────────────────────────────────────┤");
        System.out.println("│  1. Vendas simultâneas             │");
        System.out.println("│  2. Vendas consecutivas            │");
        System.out.println("└────────────────────────────────────┘");
        System.out.print("\nEscolha: ");

        String choice = reader.readLine();

        switch (choice.trim()) {
            case "1":
                System.out.print("  Produto 1: ");
                String p1 = reader.readLine();
                System.out.print("  Produto 2: ");
                String p2 = reader.readLine();
                System.out.print("  Timeout (s): ");
                long timeout = Long.parseLong(reader.readLine()) * 1000;

                stub.waitForSimultaneousSales(p1, p2, timeout, (success, happened) -> {
                    System.out.println("\n");
                    System.out.println("╔════════════════════════════════════════════════════╗");
                    System.out.println("║              🔔 NOTIFICAÇÃO RECEBIDA!              ║");
                    System.out.println("╠════════════════════════════════════════════════════╣");

                    if (success) {
                        System.out.println("║  ✓ Vendas simultâneas de: " + p1 + " e " + p2);
                    } else {
                        System.out.println("║  ✗ Timeout: vendas simultâneas não ocorreram");
                    }

                    System.out.println("╚════════════════════════════════════════════════════╝\n\n");
                });
                break;
            case "2":
                System.out.print("  Número de vendas consecutivas: ");
                int n = Integer.parseInt(reader.readLine());
                System.out.print("  Timeout (ms): ");
                long timeout2 = Long.parseLong(reader.readLine());

                stub.waitForConsecutiveSales(n, timeout2, (success, product) -> {
                    System.out.println("\n");
                    System.out.println("╔════════════════════════════════════╗");
                    System.out.println("║        🔔 NOTIFICAÇÃO RECEBIDA!    ║");
                    System.out.println("╠════════════════════════════════════╣");

                    if (success) {
                        System.out.println("║  ✓ Vendas consecutivas de: " + product);
                    } else {
                        System.out.println("║  ✗ Timeout: vendas consecutivas não ocorreram");
                    }

                    System.out.println("╚════════════════════════════════════╝\n\n");
                });
                break;
            default:
                System.out.println("Opção inválida.");
        }
    }
}
