package g8;

import java.io.BufferedInputStream;
import java.io.BufferedOutputStream;
import java.io.DataInputStream;
import java.io.DataOutputStream;
import java.io.IOException;
import java.net.Socket;
import java.util.concurrent.locks.Lock;
import java.util.concurrent.locks.ReentrantLock;

public class FramedConnection implements AutoCloseable {
    private final Socket socket;
    private final DataInputStream in;
    private final DataOutputStream out;
    private final Lock sendLock = new ReentrantLock();
    private final Lock receiveLock = new ReentrantLock();

    public FramedConnection(Socket socket) throws IOException {
        this.socket = socket;
        this.in = new DataInputStream(new BufferedInputStream(socket.getInputStream()));
        this.out = new DataOutputStream(new BufferedOutputStream(socket.getOutputStream()));
    }

    public void send(byte[] data) throws IOException {
        sendLock.lock();
        try {
            out.writeInt(data.length);
            out.write(data);
            out.flush();
        } finally {
            sendLock.unlock();
        }
    }

    public byte[] receive() throws IOException {
        receiveLock.lock();
        try {
            int length = in.readInt();
            byte[] data = new byte[length];
            in.readFully(data);
            return data;
        } finally {
            receiveLock.unlock();
        }
    }

    @Override
    public void close() throws IOException {
        socket.close();
    }
}
