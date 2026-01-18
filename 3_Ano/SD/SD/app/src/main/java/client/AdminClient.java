package client;

import java.io.*;

public class AdminClient {
    private ClientStub stub;
    private BufferedReader systemIn;

    public AdminClient(String host, int port) throws IOException, InterruptedException {
        this.stub = new ClientStub(host, port);
        this.systemIn = new BufferedReader(new InputStreamReader(System.in));
    }

    // ===================== MENU INTERATIVO =====================

    public void adminMenu() throws IOException, InterruptedException {
        boolean running = true;

        System.out.println("\n╔════════════════════════════════════╗");
        System.out.println("║   PAINEL DE ADMINISTRAÇÃO          ║");
        System.out.println("╚════════════════════════════════════╝\n");

        while (running) {
            System.out.println("\n┌────────────────────────────────────┐");
            System.out.println("│       MENU ADMINISTRADOR           │");
            System.out.println("├────────────────────────────────────┤");
            System.out.println("│  1. Avançar dia                    │");
            System.out.println("│  2. Resetar dia (voltar a hoje)    │");
            System.out.println("│  3. Ver dia atual                  │");
            System.out.println("│  4. Consultar agregações           │");
            System.out.println("│  0. Sair                           │");
            System.out.println("└────────────────────────────────────┘");
            System.out.print("\nEscolha uma opção: ");

            String choice = systemIn.readLine();

            if (choice == null || choice.trim().isEmpty()) {
                System.out.println("Opção inválida.");
                continue;
            }

            switch (choice.trim()) {
                case "1":
                    handleAdvanceDay();
                    break;
                case "2":
                    handleResetDay();
                    break;
                case "3":
                    handleCurrentDay();
                    break;
                case "4":
                    handleAggregationMenu();
                    break;
                case "0":
                    System.out.println("\nDesconectado do painel de administração.");
                    running = false;
                    break;
                default:
                    System.out.println("Opção inválida.");
            }
        }
    }

    private void handleAdvanceDay() throws IOException, InterruptedException {
        System.out.println("\n▸ Avançando para o próximo dia...");
        String newDate = stub.advanceDay();
        System.out.println("✓ Novo dia: " + newDate);
    }

    private void handleResetDay() throws IOException, InterruptedException {
        System.out.println("\n▸ Resetando simulação para hoje...");
        String currentDate = stub.resetDay();
        System.out.println("✓ Data atual: " + currentDate);
    }

    private void handleCurrentDay() throws IOException, InterruptedException {
        String info = stub.getCurrentDay();
        System.out.println("\n▸ Dia atual: " + info);
    }

    private void handleAggregationMenu() throws IOException, InterruptedException {
        System.out.println("\n┌────────────────────────────────────┐");
        System.out.println("│         AGREGAÇÕES                 │");
        System.out.println("├────────────────────────────────────┤");
        System.out.println("│  1. Quantidade vendida             │");
        System.out.println("│  2. Volume de vendas               │");
        System.out.println("│  3. Preço médio                    │");
        System.out.println("│  4. Preço máximo                   │");
        System.out.println("└────────────────────────────────────┘");
        System.out.print("\nEscolha: ");

        String choice = systemIn.readLine();

        System.out.print("  Produto: ");
        String product = systemIn.readLine();

        System.out.print("  Número de dias: ");
        int days = Integer.parseInt(systemIn.readLine());

        double result;
        String label;

        switch (choice.trim()) {
            case "1":
                result = stub.getQuantitySold(product, days);
                label = "Quantidade vendida";
                System.out.printf("\n✓ %s de '%s' nos últimos %d dias: %.2f unidades\n",
                    label, product, days, result);
                break;
            case "2":
                result = stub.getSalesVolume(product, days);
                label = "Volume de vendas";
                System.out.printf("\n✓ %s de '%s' nos últimos %d dias: %.2f€\n",
                    label, product, days, result);
                break;
            case "3":
                result = stub.getAveragePrice(product, days);
                label = "Preço médio";
                System.out.printf("\n✓ %s de '%s' nos últimos %d dias: %.2f€\n",
                    label, product, days, result);
                break;
            case "4":
                result = stub.getMaxPrice(product, days);
                label = "Preço máximo";
                System.out.printf("\n✓ %s de '%s' nos últimos %d dias: %.2f€\n",
                    label, product, days, result);
                break;
            default:
                System.out.println("Opção inválida.");
        }
    }

    public void close() throws IOException {
        stub.close();
    }


    public static void main(String[] args) {
        try {
            AdminClient admin = new AdminClient("localhost", 12346);
            admin.adminMenu();
            admin.close();
        } catch (Exception e) {
            System.err.println("Erro: " + e.getMessage());
            e.printStackTrace();
        }
    }
}
