import java.util.Scanner;

/**
 * Runner para executar todos os testes do sistema
 * Permite ao utilizador escolher qual teste executar
 */
public class TestRunner {
    public static void main(String[] args) {
        try {
            System.out.println("\n╔════════════════════════════════════════════════════╗");
            System.out.println("║         SISTEMA DISTRIBUÍDO DE EVENTOS             ║");
            System.out.println("║               SUITE DE TESTES                      ║");
            System.out.println("╚════════════════════════════════════════════════════╝\n");

            Scanner scanner = new Scanner(System.in);

            while (true) {
                printMainMenu();
                String choice = scanner.nextLine().trim();

                switch (choice) {
                    case "1":
                        runClientTests(scanner);
                        break;
                    case "2":
                        runServerTests(scanner);
                        break;
                    case "3":
                        runAllTests(scanner);
                        break;
                    case "4":
                        System.out.println("\nAté à próxima!");
                        scanner.close();
                        return;
                    default:
                        System.out.println("\n✗ Opção inválida.");
                }
            }

        } catch (Exception e) {
            System.err.println("Erro: " + e.getMessage());
            e.printStackTrace();
        }
    }

    private static void printMainMenu() {
        System.out.println("\n┌────────────────────────────────────────┐");
        System.out.println("│           MENU PRINCIPAL               │");
        System.out.println("├────────────────────────────────────────┤");
        System.out.println("│  1. Testes de Cliente (Carga)          │");
        System.out.println("│  2. Testes de Servidor (Unitários)     │");
        System.out.println("│  3. Executar Todos os Testes           │");
        System.out.println("│  4. Sair                               │");
        System.out.println("└────────────────────────────────────────┘");
        System.out.print("\nEscolha uma opção: ");
    }

    private static void runClientTests(Scanner scanner) {
        while (true) {
            System.out.println("\n┌────────────────────────────────────────┐");
            System.out.println("│        TESTES DE CLIENTE (CARGA)       │");
            System.out.println("├────────────────────────────────────────┤");
            System.out.println("│  1. Cliente Único - Pedidos            │");
            System.out.println("│     Concorrentes                       │");
            System.out.println("│  2. Múltiplos Clientes                 │");
            System.out.println("│  3. Carga Mista (Todos)                │");
            System.out.println("│  4. Voltar ao Menu Principal           │");
            System.out.println("└────────────────────────────────────────┘");
            System.out.print("\nEscolha uma opção: ");

            String choice = scanner.nextLine().trim();

            switch (choice) {
                case "1":
                    System.out.println();
                    client.ConcurrentClientTest.main(new String[] {});
                    waitForEnter(scanner);
                    break;
                case "2":
                    System.out.println();
                    client.MultiClientTest.main(new String[] {});
                    waitForEnter(scanner);
                    break;
                case "3":
                    System.out.println();
                    client.MixedWorkloadTest.main(new String[] {});
                    waitForEnter(scanner);
                    break;
                case "4":
                    return;
                default:
                    System.out.println("\n✗ Opção inválida.");
            }
        }
    }

    private static void runServerTests(Scanner scanner) {
        while (true) {
            System.out.println("\n┌────────────────────────────────────────┐");
            System.out.println("│      TESTES DE SERVIDOR (UNITÁRIOS)    │");
            System.out.println("├────────────────────────────────────────┤");
            System.out.println("│  1. AuthService (Autenticação)         │");
            System.out.println("│  2. EventSerializer (Serialização)     │");
            System.out.println("│  3. EventService (Eventos)             │");
            System.out.println("│  4. SeriesCache (Cache)                │");
            System.out.println("│  5. Executar Todos os Testes Server    │");
            System.out.println("│  6. Voltar ao Menu Principal           │");
            System.out.println("└────────────────────────────────────────┘");
            System.out.print("\nEscolha uma opção: ");

            String choice = scanner.nextLine().trim();

            switch (choice) {
                case "1":
                    System.out.println("\n📋 Executando testes de AuthService...\n");
                    runJUnitTest("server.AuthServiceTestSuite");
                    waitForEnter(scanner);
                    break;
                case "2":
                    System.out.println("\n📋 Executando testes de EventSerializer...\n");
                    runJUnitTest("server.EventSerializerTestSuite");
                    waitForEnter(scanner);
                    break;
                case "3":
                    System.out.println("\n📋 Executando testes de EventService...\n");
                    runJUnitTest("server.EventServiceTest");
                    waitForEnter(scanner);
                    break;
                case "4":
                    System.out.println("\n📋 Executando testes de SeriesCache...\n");
                    runJUnitTest("server.SeriesCacheTestSuite");
                    waitForEnter(scanner);
                    break;
                case "5":
                    System.out.println("\n📋 Executando todos os testes de servidor...\n");
                    runAllServerTests();
                    waitForEnter(scanner);
                    break;
                case "6":
                    return;
                default:
                    System.out.println("\n✗ Opção inválida.");
            }
        }
    }

    private static void runAllTests(Scanner scanner) {
        System.out.println("\n╔════════════════════════════════════════════════════╗");
        System.out.println("║           EXECUTANDO TODOS OS TESTES               ║");
        System.out.println("╚════════════════════════════════════════════════════╝\n");

        // Testes de servidor (unitários)
        System.out.println("━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━");
        System.out.println("             TESTES DE SERVIDOR (UNITÁRIOS)          ");
        System.out.println("━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━\n");
        runAllServerTests();

        // Testes de cliente (carga)
        System.out.println("\n━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━");
        System.out.println("             TESTES DE CLIENTE (CARGA)               ");
        System.out.println("━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━\n");

        System.out.println("▶ Teste 1: Cliente Único - Pedidos Concorrentes");
        client.ConcurrentClientTest.main(new String[] {});

        System.out.println("\n▶ Teste 2: Múltiplos Clientes");
        client.MultiClientTest.main(new String[] {});

        System.out.println("\n▶ Teste 3: Carga Mista");
        client.MixedWorkloadTest.main(new String[] {});

        System.out.println("\n╔════════════════════════════════════════════════════╗");
        System.out.println("║         TODOS OS TESTES CONCLUÍDOS                 ║");
        System.out.println("╚════════════════════════════════════════════════════╝");

        waitForEnter(scanner);
    }

    private static void runAllServerTests() {
        String[] testClasses = {
                "server.AuthServiceTestSuite",
                "server.EventSerializerTestSuite",
                "server.EventServiceTest",
                "server.SeriesCacheTestSuite"
        };

        for (String testClass : testClasses) {
            runJUnitTest(testClass);
            System.out.println();
        }
    }

    private static void runJUnitTest(String testClassName) {
        System.out.println("╔════════════════════════════════════════════════════╗");
        System.out.println("║  TESTE: " + padRight(testClassName, 42) + "║");
        System.out.println("╚════════════════════════════════════════════════════╝\n");

        try {
            // Usar reflexão para encontrar e executar testes JUnit
            Class<?> testClass = Class.forName(testClassName);

            // Contar métodos de teste
            java.lang.reflect.Method[] methods = testClass.getDeclaredMethods();
            int testCount = 0;
            int passCount = 0;
            int failCount = 0;

            // Criar instância
            Object instance = testClass.getDeclaredConstructor().newInstance();

            // Procurar método setUp (@BeforeEach)
            java.lang.reflect.Method setUp = null;
            for (java.lang.reflect.Method m : methods) {
                if (m.isAnnotationPresent(org.junit.jupiter.api.BeforeEach.class)) {
                    setUp = m;
                    break;
                }
            }

            // Executar cada método de teste
            for (java.lang.reflect.Method method : methods) {
                if (method.isAnnotationPresent(org.junit.jupiter.api.Test.class)) {
                    testCount++;
                    String testName = method.getName();

                    // Verificar se tem @DisplayName
                    org.junit.jupiter.api.DisplayName displayName = method
                            .getAnnotation(org.junit.jupiter.api.DisplayName.class);
                    if (displayName != null) {
                        testName = displayName.value();
                    }

                    try {
                        // Executar setUp antes de cada teste
                        if (setUp != null) {
                            setUp.invoke(instance);
                        }

                        // Executar teste
                        method.invoke(instance);
                        passCount++;
                        System.out.println("  ✓ " + testName);
                    } catch (Exception e) {
                        failCount++;
                        Throwable cause = e.getCause() != null ? e.getCause() : e;
                        System.out.println("  ✗ " + testName);
                        System.out.println("    └─ " + cause.getClass().getSimpleName() + ": " + cause.getMessage());
                    }
                }
            }

            // Resumo
            System.out.println();
            System.out.println("┌────────────────────────────────────────┐");
            System.out.printf("│ Testes: %d | Passou: %d | Falhou: %d      │%n", testCount, passCount, failCount);
            if (failCount == 0) {
                System.out.println("│ RESULTADO: ✓ TODOS PASSARAM           │");
            } else {
                System.out.println("│ RESULTADO: ✗ ALGUNS FALHARAM          │");
            }
            System.out.println("└────────────────────────────────────────┘");

        } catch (ClassNotFoundException e) {
            System.out.println("  ✗ Classe de teste não encontrada: " + testClassName);
        } catch (Exception e) {
            System.out.println("  ✗ Erro ao executar testes: " + e.getMessage());
            e.printStackTrace();
        }
    }

    private static void waitForEnter(Scanner scanner) {
        System.out.println("\nPressione Enter para continuar...");
        scanner.nextLine();
    }

    private static String padRight(String s, int n) {
        return String.format("%-" + n + "s", s);
    }
}
