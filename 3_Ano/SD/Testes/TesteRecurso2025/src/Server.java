import java.io.*;
import java.net.ServerSocket;
import java.net.Socket;
import java.sql.SQLIntegrityConstraintViolationException;
import java.util.Map;

public class Server {
    public static void main(String[] args) throws Exception {
        ServerSocket serverSocket = new ServerSocket(12345);
        int N = 5; // Number of galleries
        int C = 10; // Capacity per gallery
        MuseumManager mm = new MuseumManager(N, C);

        while (true) {
            var socket = serverSocket.accept();
            new Thread(new ClientHandler(socket, mm)).start();
        }
    }

    static class ClientHandler implements Runnable {
        private final Socket socket;
        private final MuseumManager mm;

        public ClientHandler(java.net.Socket socket, MuseumManager mm) {
            this.socket = socket;
            this.mm = mm;
        }

        @Override
        public void run() {
            try (
                    BufferedReader in = new BufferedReader(new InputStreamReader(socket.getInputStream()));
                    PrintWriter out = new PrintWriter(socket.getOutputStream());
            ) {
                String cmd;
                while ((cmd = in.readLine()) != null) {
                    String[] parts = cmd.split(" ");
                    String response = "";
                    switch (parts[0]) {
                        case "buyTicket":
                            int uses = Integer.parseInt(parts[1]);
                            response = mm.buyTicket(uses);
                            break;
                        case "enterGallery":
                            int galleryId = Integer.parseInt(parts[1]);
                            String ticketId = parts[2];
                            try {
                                int result = mm.enterGallery(galleryId, ticketId);
                                response = Integer.toString(result);
                            } catch (InterruptedException e) {
                                response = "ERROR";
                            }
                            break;
                        case "exitGallery":
                            int galId = Integer.parseInt(parts[1]);
                            String tickId = parts[2];
                            mm.exitGallery(galId, tickId);
                            response = "OK";
                            break;
                        case "peopleWaitingPerGallery":
                            Map<Integer, Integer> map = mm.peopleWaitingPerGallery();
                            StringBuilder sb = new StringBuilder();
                            for (Map.Entry<Integer, Integer> entry : map.entrySet()) {
                                sb.append(entry.getKey()).append(":").append(entry.getValue()).append("\n");
                            }
                            response = sb.toString().trim();
                            break;
                        default:
                            response = "ERROR: unknown command";
                    }
                    out.println(response);
                    out.flush();
                }
            } catch (IOException e) {
                // Client disconnected
            }
        }
    }
}
