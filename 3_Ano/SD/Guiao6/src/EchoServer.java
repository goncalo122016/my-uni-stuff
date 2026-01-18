import java.io.BufferedReader;
import java.io.IOException;
import java.io.InputStreamReader;
import java.io.PrintWriter;
import java.net.ServerSocket;
import java.net.Socket;
import java.util.ArrayList;
import java.util.List;
import java.util.concurrent.locks.ReentrantLock;

public class EchoServer {

    public static class ServerInfo {
        private int mediaClientes = 0;
        private int nclientes = 0;
        private ReentrantLock lock = new ReentrantLock();

        public ServerInfo(int mediaClientes) {
            this.mediaClientes = mediaClientes;
        }

        public int getMedia(int myMedia) {
            lock.lock();
            try {
                nclientes++;
                mediaClientes = (mediaClientes + myMedia) / nclientes;
                return mediaClientes;
            } finally {
                lock.unlock();
            }
        }
    }

    static class ClientHandler extends Thread {
        private Socket socket;
        private ServerInfo serverInfo;

        public ClientHandler(Socket socket, ServerInfo si) {
            this.socket = socket;
            this.serverInfo = si;
        }

        @Override
        public void run() {
            try {
                BufferedReader in = new BufferedReader(new InputStreamReader(socket.getInputStream()));
                PrintWriter out = new PrintWriter(socket.getOutputStream());

                String line;
                List<Integer> numbers = new ArrayList<Integer>();
                int n;

                while ((line = in.readLine()) != null) {
                    n = Integer.parseInt(line);
                    numbers.add(n);
                    Integer sum = numbers.stream().mapToInt(Integer::intValue).sum();
                    System.out.println("Received from " + socket.getRemoteSocketAddress() + ": " + n + ", Numbers: " + numbers);
                    out.println(sum);
                    out.flush();
                }

                int media = numbers.stream().mapToInt(Integer::intValue).sum() / numbers.size();
                out.println("Connection closed by client. Média de TODOS os Clientes: " + serverInfo.getMedia(media));
                out.flush();

                socket.shutdownOutput();
                socket.shutdownInput();
                socket.close();
            } catch (IOException e) {
                e.printStackTrace();
            }
        }
    }

    public static void main(String[] args) {
        try {
            ServerSocket ss = new ServerSocket(12345);
            ServerInfo si = new ServerInfo(0);

            while (true) {
                Socket socket = ss.accept();
                ClientHandler clientHandler = new ClientHandler(socket, si);
                clientHandler.start();
            }
        } catch (IOException e) {
            e.printStackTrace();
        }
    }
}
