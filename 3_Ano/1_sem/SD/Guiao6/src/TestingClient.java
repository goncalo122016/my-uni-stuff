import java.io.BufferedReader;
import java.io.IOException;
import java.io.InputStreamReader;
import java.io.PrintWriter;
import java.net.Socket;

public class TestingClient {
    public static void main(String[] args) throws IOException {
        Socket socket = new Socket ("localhost", 12345);

        BufferedReader in = new BufferedReader(new InputStreamReader(socket.getInputStream()));
        PrintWriter out = new PrintWriter(socket.getOutputStream(), false);
        BufferedReader stdin = new BufferedReader(new InputStreamReader(System.in));

        String message;
        while ((message = stdin.readLine()) != null) {
            out.println(message);
            out.flush();
            String response = in.readLine();
            System.out.println("Server response: " + response);
        }

        socket.shutdownInput();
        socket.shutdownOutput();
        socket.close();
    }
}