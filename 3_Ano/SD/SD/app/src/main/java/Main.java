import client.Client;
import client.AdminClient;
import java.io.BufferedReader;
import java.io.InputStreamReader;

public class Main {
    public static void main(String[] args) {
        try {
            System.out.println("========================================");
            System.out.println("  SISTEMA CLIENTE-SERVIDOR SD");
            System.out.println("========================================");
            System.out.println("");
            System.out.println("Selecione o tipo de cliente:");
            System.out.println("1. Cliente Normal (adicionar eventos)");
            System.out.println("2. Cliente Admin (gerenciar tempo/dias)");
            System.out.println("");
            System.out.print("Escolha uma opção (1 ou 2): ");

            BufferedReader reader = new BufferedReader(new InputStreamReader(System.in));
            String choice = reader.readLine();

            System.out.println("");

            if (choice == null || choice.trim().isEmpty()) {
                System.out.println("Opção inválida. A executar cliente normal...");
                Client.main(new String[] {});
            } else if (choice.trim().equals("1")) {
                System.out.println("Iniciando cliente normal...");
                System.out.println("(Conectando a localhost:12345)");
                System.out.println("");
                Client.main(new String[] {});
            } else if (choice.trim().equals("2")) {
                System.out.println("Iniciando painel de administração...");
                System.out.println("(Conectando a localhost:12346)");
                System.out.println("");
                AdminClient.main(new String[] {});
            } else {
                System.out.println("Opção inválida. A executar cliente normal...");
                Client.main(new String[] {});
            }

        } catch (Exception e) {
            System.err.println("Erro ao inicializar cliente: " + e.getMessage());
            e.printStackTrace();
        }
    }
}
