package client;


import java.io.IOException;
import java.util.ArrayList;
import java.util.List;
import java.util.concurrent.ExecutorService;
import java.util.concurrent.Executors;
import java.util.concurrent.TimeUnit;
import java.util.concurrent.locks.ReentrantLock;


/**
 * Teste: Vários clientes fazendo um pedido ao mesmo tempo
 *
 * Objetivo: Verificar se múltiplos clientes conseguem fazer pedidos
 * simultaneamente sem conflitos ou condições de corrida.
 * Teste de stress com 10000 clientes.
 */
public class MultiClientTest {
    private static final String HOST = "localhost";
    private static final int PORT = 12345;
    private static final int NUM_CLIENTS = 20000;
    private static final int OPERATIONS_PER_CLIENT = 5;


    public static void main(String[] args) {
        try {
            System.out.println("╔════════════════════════════════════════════════════╗");
            System.out.println("║   TESTE: Múltiplos Clientes Simultâneos           ║");
            System.out.println("║         (Teste de Stress - 50000 clientes)        ║");
            System.out.println("╚════════════════════════════════════════════════════╝\n");


            MultiClientTest test = new MultiClientTest();
            boolean result = test.runTest();


            if (result) {
                System.out.println("\n✓ Teste PASSOU!");
            } else {
                System.out.println("\n✗ Teste FALHOU!");
            }


        } catch (Exception e) {
            System.err.println("✗ Erro ao executar teste: " + e.getMessage());
            e.printStackTrace();
        }
    }


    /**
     * Executa o teste: múltiplos clientes
     */
    public boolean runTest() throws IOException, InterruptedException {
        System.out.println("Configuração:");
        System.out.println("  - Servidor: " + HOST + ":" + PORT);
        System.out.println("  - Número de clientes: " + NUM_CLIENTS);
        System.out.println("  - Operações por cliente: " + OPERATIONS_PER_CLIENT);
        System.out.println("  - Total de operações: " + (NUM_CLIENTS * OPERATIONS_PER_CLIENT));
        System.out.println();


        // Verificar se servidor está online
        if (!isServerOnline()) {
            System.err.println("✗ Servidor não está online!");
            return false;
        }


        System.out.println("✓ Servidor está online\n");


        // Criar pool de threads (ajustado para melhor performance com muitos clientes)
        int threadPoolSize = Math.min(NUM_CLIENTS, 500); // Limitar pool de threads
        ExecutorService executorService = Executors.newFixedThreadPool(threadPoolSize);
        List<Long> responseTimes = new ArrayList<>();
        ReentrantLock timesLock = new ReentrantLock();
        int[] successCount = { 0 };
        int[] failureCount = { 0 };
        int[] completedClients = { 0 };


        long testStartTime = System.currentTimeMillis();


        System.out.println("[1/3] Criando " + NUM_CLIENTS + " clientes...");
        System.out.println("      (Pool de threads: " + threadPoolSize + ")\n");


        // Criar clientes
        for (int clientId = 0; clientId < NUM_CLIENTS; clientId++) {
            final int finalClientId = clientId;
            executorService.submit(() -> {
                try {
                    // Cada cliente cria sua própria conexão
                    final ClientStub stub = new ClientStub(HOST, PORT);


                    // Login
                    String username = "user_" + finalClientId;
                    String password = "pass_" + finalClientId;


                    boolean loggedIn = false;
                    try {
                        loggedIn = stub.logIn(username, password);
                        if (!loggedIn) {
                            stub.signIn(username, password);
                            loggedIn = stub.logIn(username, password);
                        }
                    } catch (InterruptedException e) {
                        System.err
                                .println("✗ Cliente " + finalClientId + ": erro ao fazer login (InterruptedException)");
                        timesLock.lock();
                        try {
                            failureCount[0] += OPERATIONS_PER_CLIENT;
                        } finally {
                            timesLock.unlock();
                        }
                        stub.close();
                        return;
                    }


                    if (!loggedIn) {
                        System.err.println("✗ Cliente " + finalClientId + ": falha no login");
                        timesLock.lock();
                        try {
                            failureCount[0] += OPERATIONS_PER_CLIENT;
                        } finally {
                            timesLock.unlock();
                        }
                        stub.close();
                        return;
                    }


                    // Executar operações
                    for (int op = 0; op < OPERATIONS_PER_CLIENT; op++) {
                        try {
                            long startTime = System.nanoTime();


                            String product = "Product_" + (finalClientId % 10);
                            double quantity = 1.0 + (op % 5);
                            double price = 100.0 + (op % 50);


                            boolean success = stub.insertEvent(product, quantity, price);


                            long endTime = System.nanoTime();
                            long duration = (endTime - startTime) / 1_000_000; // ms


                            timesLock.lock();
                            try {
                                responseTimes.add(duration);
                                if (success) {
                                    successCount[0]++;
                                } else {
                                    failureCount[0]++;
                                }
                            } finally {
                                timesLock.unlock();
                            }


                        } catch (Exception e) {
                            System.err.println("✗ Cliente " + finalClientId + ": erro na operação " + op);
                            timesLock.lock();
                            try {
                                failureCount[0]++;
                            } finally {
                                timesLock.unlock();
                            }
                        }
                    }


                    stub.close();

                    // Progresso
                    timesLock.lock();
                    try {
                        completedClients[0]++;
                        if (completedClients[0] % 1000 == 0) {
                            System.out.println("      Progresso: " + completedClients[0] + "/" + NUM_CLIENTS + 
                                             " clientes completados (" + 
                                             String.format("%.1f%%", completedClients[0] * 100.0 / NUM_CLIENTS) + ")");
                        }
                    } finally {
                        timesLock.unlock();
                    }


                } catch (IOException e) {
                    System.err.println("✗ Cliente " + finalClientId + ": erro de conexão - " + e.getMessage());
                    timesLock.lock();
                    try {
                        failureCount[0] += OPERATIONS_PER_CLIENT;
                    } finally {
                        timesLock.unlock();
                    }
                }
            });
        }


        // Aguardar conclusão
        System.out.println("[2/3] Aguardando conclusão de todos os clientes...");
        executorService.shutdown();
        if (!executorService.awaitTermination(10, TimeUnit.MINUTES)) { // Aumentado timeout para 10 minutos
            System.err.println("✗ Timeout ao aguardar conclusão!");
            executorService.shutdownNow();
            return false;
        }


        long testEndTime = System.currentTimeMillis();
        long totalTime = testEndTime - testStartTime;


        // Análise de resultados
        System.out.println("\n[3/3] Gerando relatório...\n");
        System.out.println("╔════════════════════════════════════════════════════╗");
        System.out.println("║                   RESULTADOS                       ║");
        System.out.println("╠════════════════════════════════════════════════════╣");
        System.out.printf("║ Total de operações: %d\n", (successCount[0] + failureCount[0]));
        System.out.printf("║ Sucessos:          %d ✓\n", successCount[0]);
        System.out.printf("║ Falhas:            %d ✗\n", failureCount[0]);
        System.out.printf("║ Taxa de sucesso:   %.1f%%\n",
                (successCount[0] * 100.0 / (successCount[0] + failureCount[0])));
        System.out.printf("║ Tempo total:       %d ms (%.2f seg)\n", totalTime, totalTime / 1000.0);
        if (successCount[0] > 0) {
            System.out.printf("║ Operações/segundo: %.2f\n", (successCount[0] * 1000.0 / totalTime));
        }


        if (!responseTimes.isEmpty()) {
            long avgTime = responseTimes.stream().mapToLong(Long::longValue).sum() / responseTimes.size();
            long maxTime = responseTimes.stream().mapToLong(Long::longValue).max().orElse(0);
            long minTime = responseTimes.stream().mapToLong(Long::longValue).min().orElse(0);


            System.out.printf("║ Tempo médio:       %d ms\n", avgTime);
            System.out.printf("║ Tempo máx:         %d ms\n", maxTime);
            System.out.printf("║ Tempo mín:         %d ms\n", minTime);
        }
        System.out.println("╠════════════════════════════════════════════════════╣");


        boolean passed = failureCount[0] == 0 && successCount[0] > 0;
        if (passed) {
            System.out.println("║ RESULTADO: ✓ TESTE PASSOU COM SUCESSO           ║");
        } else if (successCount[0] > 0) {
            System.out.println("║ RESULTADO: ⚠ TESTE PARCIALMENTE BEM-SUCEDIDO     ║");
        } else {
            System.out.println("║ RESULTADO: ✗ TESTE FALHOU COMPLETAMENTE         ║");
        }
        System.out.println("╚════════════════════════════════════════════════════╝");


        return passed;
    }


    private boolean isServerOnline() {
        try {
            ClientStub testStub = new ClientStub(HOST, PORT);
            testStub.close();
            return true;
        } catch (IOException e) {
            return false;
        }
    }
}