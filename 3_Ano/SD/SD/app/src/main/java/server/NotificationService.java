package server;

import java.util.*;
import java.util.concurrent.locks.Condition;
import java.util.concurrent.locks.ReentrantLock;

/**
 * Serviço de notificações para vendas simultâneas e consecutivas.
 *
 * - Vendas Simultâneas: bloqueia até p1 e p2 serem vendidos no mesmo dia
 * - Vendas Consecutivas: bloqueia até n vendas consecutivas do mesmo produto
 */
public class NotificationService {
    private final ReentrantLock lock = new ReentrantLock();
    private final Condition dayChanged = lock.newCondition();
    private boolean dayHasEnded = false;

    // Rastreia último produto vendido para detectar consecutivas
    private String lastProductSold = null;
    private int consecutiveCount = 0;

    // Rastreia produtos vendidos no dia corrente
    private final Set<String> productsToday = new HashSet<>();

    /**
     * Registra um novo evento (chamado pelo EventService ao adicionar evento).
     * Notifica clientes a aguardar vendas simultâneas/consecutivas.
     */
    public void recordEvent(String productName) {
        lock.lock();
        try {
            // Atualizar produtos do dia
            productsToday.add(productName);

            // Atualizar contador de consecutivas
            if (productName.equals(lastProductSold)) {
                consecutiveCount++;
            } else {
                lastProductSold = productName;
                consecutiveCount = 1;
            }

            // Acordar todos os clientes a aguardar notificações
            dayChanged.signalAll();
        } finally {
            lock.unlock();
        }
    }

    /**
     * Bloqueia até que dois produtos específicos sejam vendidos no mesmo dia.
     *
     * @param p1        Primeiro produto
     * @param p2        Segundo produto
     * @param maxWaitMs Tempo máximo de espera (em ms). Se <= 0, aguarda até fim do
     *                  dia
     * @return true se ambos foram vendidos, false se o dia terminou sem tal
     */
    public boolean waitForSimultaneousSales(String p1, String p2, long maxWaitMs)
            throws InterruptedException {
        if (p1 == null || p2 == null || p1.isEmpty() || p2.isEmpty()) {
            return false;
        }

        lock.lock();
        try {
            long deadline = maxWaitMs > 0 ? System.currentTimeMillis() + maxWaitMs : Long.MAX_VALUE;

            while (true) {

                if (dayHasEnded) {
                    return false;
                }

                // Verificar se ambos os produtos foram vendidos
                if (productsToday.contains(p1) && productsToday.contains(p2)) {
                    return true;
                }

                // Calcular tempo de espera
                long remaining = deadline - System.currentTimeMillis();
                if (remaining <= 0 && maxWaitMs > 0) {
                    // Timeout ou fim do dia
                    return false;
                }

                // Aguardar notificação (com timeout se especificado)
                if (maxWaitMs > 0) {
                    remaining = Math.max(1, remaining);
                    if (!dayChanged.await(remaining, java.util.concurrent.TimeUnit.MILLISECONDS)) {
                        return false; // Timeout
                    }
                } else {
                    dayChanged.await();
                }
            }
        } finally {
            lock.unlock();
        }
    }

    /**
     * Bloqueia até que n vendas consecutivas do mesmo produto ocorram.
     *
     * @param n         Número de vendas consecutivas
     * @param maxWaitMs Tempo máximo de espera (em ms). Se <= 0, aguarda até fim do
     *                  dia
     * @return Nome do produto se conseguiu n consecutivas, null se o dia terminou
     */
    public String waitForConsecutiveSales(int n, long maxWaitMs)
            throws InterruptedException {
        if (n <= 0) {
            return null;
        }

        lock.lock();
        try {
            long deadline = maxWaitMs > 0 ? System.currentTimeMillis() + maxWaitMs : Long.MAX_VALUE;

            while (true) {

                if (dayHasEnded) {
                    return null;
                }

                // Verificar se atingiu n consecutivas
                if (consecutiveCount >= n) {
                    return lastProductSold;
                }

                // Calcular tempo de espera
                long remaining = deadline - System.currentTimeMillis();
                if (remaining <= 0 && maxWaitMs > 0) {
                    // Timeout ou fim do dia
                    return null;
                }

                // Aguardar notificação (com timeout se especificado)
                if (maxWaitMs > 0) {
                    remaining = Math.max(1, remaining);
                    if (!dayChanged.await(remaining, java.util.concurrent.TimeUnit.MILLISECONDS)) {
                        return null; // Timeout
                    }
                } else {
                    dayChanged.await();
                }
            }
        } finally {
            lock.unlock();
        }
    }

    /**
     * Inicia um novo dia (chamado DEPOIS de advanceDay).
     * Reseta todos os contadores e flags.
     */
    public void startNewDay() {
        lock.lock();
        try {
            System.out.println("[NotificationService] startNewDay() chamado");

            productsToday.clear();
            lastProductSold = null;
            consecutiveCount = 0;
            dayHasEnded = false;

        } finally {
            lock.unlock();
        }
    }

    /**
     * Termina o dia atual (chamado ANTES de advanceDay).
     * Notifica todas as threads aguardando que o dia terminou.
     */
    public void endDay() {
        lock.lock();
        try {
            System.out.println("[NotificationService] endDay() chamado");

            dayHasEnded = true;

            dayChanged.signalAll();

        } finally {
            lock.unlock();
        }
    }

    /**
     * Obtem os produtos vendidos no dia corrente.
     */
    public Set<String> getTodayProducts() {
        lock.lock();
        try {
            return new HashSet<>(productsToday);
        } finally {
            lock.unlock();
        }
    }

    /**
     * Obtem o último produto vendido.
     */
    public String getLastProduct() {
        lock.lock();
        try {
            return lastProductSold;
        } finally {
            lock.unlock();
        }
    }

    /**
     * Obtem o contador de vendas consecutivas.
     */
    public int getConsecutiveCount() {
        lock.lock();
        try {
            return consecutiveCount;
        } finally {
            lock.unlock();
        }
    }
}
