package client;

/**
 * Callback chamado quando uma notificação é recebida.
 */
@FunctionalInterface
public interface NotificationCallback {
    /**
     * Chamado quando a notificação é recebida.
     *
     * @param success true se a condição foi atingida, false se timeout/dia terminou
     * @param productName nome do produto(s), null se não aplicável
     */
    void onNotification(boolean success, String productName);
}