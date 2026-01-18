package server;

import protocol.*;
import java.net.*;
import java.io.*;
import java.util.HashMap;
import common.ThreadPool;
import server.event.EventService;
import server.workers.*;

public class Server {

    public static void main(String[] args) {
        try (ServerSocket ss = new ServerSocket(12345)) {

            int D = 30;
            int S = 10;
            if (args.length > 0) {
                try {
                    D = Integer.parseInt(args[0]);
                    S = Integer.parseInt(args[1]);
                } catch (NumberFormatException e) {
                    System.out.println("[INIT] Parâmetro D inválido, usando 30");
                }
            }

            System.out.println("[INIT] Parâmetro D configurado para: " + D + " dias");

            // Serviços partilhados
            HashMap<String, String> usersMap = new HashMap<>();
            AuthService authService = new AuthService(usersMap);
            EventService eventService = new EventService(S, D);
            AggregationService aggregationService = new AggregationService(eventService, D);
            NotificationService notificationService = eventService.getNotificationService();

            // Pools de threads
            ThreadPool authPool = new ThreadPool(2);
            ThreadPool adminPool = new ThreadPool(2);
            ThreadPool filterPool = new ThreadPool(4);
            ThreadPool notifyPool = new ThreadPool(4);
            ThreadPool insertPool = new ThreadPool(4);

            System.out.println("Servidor iniciado na porta 12345...");

            // ============= INICIA SERVIDOR ADMIN (porta 12346) =============
            final int finalD = D;
            new Thread(() -> {
                try (ServerSocket adminSS = new ServerSocket(12346)) {
                    System.out.println("[ADMIN] Servidor de administração iniciado na porta 12346...");

                    while (true) {
                        Socket s = adminSS.accept();
                        System.out.println("[ADMIN] Nova ligação admin: " + s.getInetAddress());

                        // Cria handler binário para admin
                        new Thread(new AdminHandler(s, eventService, aggregationService, finalD)).start();
                    }
                } catch (IOException e) {
                    System.err.println("[ADMIN] Erro: " + e.getMessage());
                }
            }, "AdminServerThread").start();

            while (true) {
                Socket s = ss.accept();
                System.out.println("Novo cliente ligado: " + s.getInetAddress());

                TaggedConnection conn = new TaggedConnection(s);

                // Dispatcher para clientes normais
                new Thread(() -> dispatchLoop(
                        conn,
                        authPool, adminPool, filterPool, notifyPool, insertPool,
                        authService, eventService, aggregationService, notificationService),
                        "Dispatcher-" + s.getInetAddress()).start();
            }

        } catch (IOException e) {
            e.printStackTrace();
        }
    }

    private static void dispatchLoop(
            TaggedConnection conn,
            ThreadPool authPool,
            ThreadPool adminPool,
            ThreadPool filterPool,
            ThreadPool notifyPool,
            ThreadPool insertPool,
            AuthService authService,
            EventService eventService,
            AggregationService aggregationService,
            NotificationService notificationService) {

        try {
            while (true) {
                Frame f = conn.receive();

                switch (f.type()) {
                    case AUTH -> authPool.submit(() -> AuthWorker.handle(f, conn, authService));
                    case ADMIN -> adminPool.submit(() -> AdminWorker.handle(f, conn, eventService, aggregationService));
                    case FILTER -> filterPool.submit(() -> FilterWorker.handle(f, conn, eventService));
                    case NOTIFY -> notifyPool.submit(() -> NotificationWorker.handle(f, conn, notificationService));
                    case INSERT -> insertPool.submit(() -> InsertWorker.handle(f, conn, eventService));
                }
            }
        } catch (IOException e) {
            System.out.println("[Dispatcher] Ligação terminada: " + e.getMessage());
        } finally {
            try {
                conn.close();
            } catch (IOException ignored) {
            }
        }
    }
}
