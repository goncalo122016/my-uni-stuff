package client;


import java.io.IOException;
import java.util.ArrayList;
import java.util.List;
import java.util.concurrent.ExecutorService;
import java.util.concurrent.Executors;
import java.util.concurrent.TimeUnit;
import java.util.concurrent.locks.ReentrantLock;


/**
 * Teste: Um cliente a fazer múltiplos pedidos concorrentes
 *
 * Objetivo: Verificar se um único cliente consegue enviar múltiplos pedidos
 * ao mesmo tempo sem bloqueios ou deadlocks.
 * 
 * TESTE EXAUSTIVO: 10000 operações com 100 threads concorrentes
 */
public class ConcurrentClientTest {
    private static final String HOST = "localhost";
    private static final int PORT = 12345;
    private static final int NUM_OPERATIONS = 10000;
    private static final int NUM_THREADS = 100;


    public static void main(String[] args) {
        try {
            System.out.println("╔════════════════════════════════════════════════════╗");
            System.out.println("║   TESTE: Um Cliente com Pedidos Concorrentes      ║");
            System.out.println("║                TESTE EXAUSTIVO                     ║");
            System.out.println("╚════════════════════════════════════════════════════╝\n");


            ConcurrentClientTest test = new ConcurrentClientTest();
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
     * Executa o teste: um cliente com múltiplos pedidos concorrentes
     */
    public boolean runTest() throws IOException, InterruptedException {
        System.out.println("Configuração:");
        System.out.println("  - Servidor: " + HOST + ":" + PORT);
        System.out.println("  - Operações por thread: " + NUM_OPERATIONS);
        System.out.println("  - Threads concorrentes: " + NUM_THREADS);
        System.out.println("  - Total de operações: " + (NUM_OPERATIONS * NUM_THREADS));
        System.out.println();


        // Verificar se servidor está online
        if (!isServerOnline()) {
            System.err.println("✗ Servidor não está online!");
            return false;
        }


        System.out.println("✓ Servidor está online\n");


        // Criar um único cliente stub
        final ClientStub stub;
        try {
            System.out.println("[1/4] Conectando ao servidor " + HOST + ":" + PORT + "...");
            stub = new ClientStub(HOST, PORT);
            System.out.println("      ✓ Conexão estabelecida");


            // Fazer login com timeout
            System.out.println("[2/4] Fazendo login com testuser...");
            boolean loggedIn = false;
            try {
                loggedIn = stub.logIn("testuser", "testpass123");
                if (!loggedIn) {
                    System.out.println("      → Login falhou, tentando criar nova conta...");
                    stub.signIn("testuser", "testpass123");
                    System.out.println("      ✓ Conta criada, fazendo login novamente...");
                    loggedIn = stub.logIn("testuser", "testpass123");
                }
            } catch (InterruptedException e) {
                System.err.println("✗ Login interrompido!");
                stub.close();
                return false;
            }


            if (!loggedIn) {
                System.err.println("✗ Falha no login!");
                stub.close();
                return false;
            }
            System.out.println("      ✓ Login bem-sucedido\n");
        } catch (IOException e) {
            System.err.println("✗ Erro ao conectar: " + e.getMessage());
            e.printStackTrace();
            return false;
        }


        // Criar pool de threads para executar operações concorrentes
        ExecutorService executorService = Executors.newFixedThreadPool(NUM_THREADS);
        List<Long> responseTimes = new ArrayList<>();
        ReentrantLock timesLock = new ReentrantLock();
        int[] successCount = { 0 };
        int[] failureCount = { 0 };


        long testStartTime = System.currentTimeMillis();
        int totalOperations = NUM_OPERATIONS * NUM_THREADS;


        System.out.println("[3/4] Executando " + totalOperations + " operações concorrentes com " 
                         + NUM_THREADS + " threads...");
        System.out.println("      (Isso pode demorar alguns minutos...)\n");


        // Submeter operações
        for (int i = 0; i < totalOperations; i++) {
            final int operationId = i;
            executorService.submit(() -> {
                try {
                    long startTime = System.nanoTime();


                    // Operação: inserir evento
                    String product = "Product_" + (operationId % 10);
                    double quantity = 1.0 + (operationId % 5);
                    double price = 100.0 + (operationId % 50);


                    boolean success = stub.insertEvent(product, quantity, price);


                    long endTime = System.nanoTime();
                    long duration = (endTime - startTime) / 1_000_000; // Converter para ms


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


                    // Progresso a cada 10000 operações
                    if (operationId % 10000 == 0 && operationId > 0) {
                        double percentage = (operationId * 100.0) / totalOperations;
                        System.out.printf("      Progresso: %d/%d (%.1f%%) - Sucessos: %d, Falhas: %d\n",
                                        operationId, totalOperations, percentage, 
                                        successCount[0], failureCount[0]);
                    }


                } catch (Exception e) {
                    System.err.println("  ✗ Erro na operação " + operationId + ": " + e.getMessage());
                    timesLock.lock();
                    try {
                        failureCount[0]++;
                    } finally {
                        timesLock.unlock();
                    }
                }
            });
        }


        // Aguardar conclusão
        System.out.println("\n      Aguardando conclusão de todas as threads...");
        executorService.shutdown();
        if (!executorService.awaitTermination(30, TimeUnit.MINUTES)) {
            System.err.println("✗ Timeout ao aguardar conclusão das operações!");
            executorService.shutdownNow();
            stub.close();
            return false;
        }


        long testEndTime = System.currentTimeMillis();
        long totalTime = testEndTime - testStartTime;


        // Fechar conexão
        System.out.println("\n[4/4] Fechando conexão...");
        stub.close();
        System.out.println("      ✓ Conexão fechada\n");


        // Análise de resultados
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


    /**
     * Verifica se o servidor está online
     */
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