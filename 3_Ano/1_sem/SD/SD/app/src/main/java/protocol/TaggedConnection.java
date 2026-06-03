package protocol;

import java.io.*;
import java.net.Socket;
import java.util.concurrent.locks.Lock;
import java.util.concurrent.locks.ReentrantLock;

public class TaggedConnection implements AutoCloseable {

    private final Socket socket;
    private final DataInputStream in;
    private final DataOutputStream out;
    private final Lock sendLock = new ReentrantLock();
    private final Lock receiveLock = new ReentrantLock();

    public TaggedConnection(Socket socket) throws IOException {
        this.socket = socket;
        this.in = new DataInputStream(new BufferedInputStream(socket.getInputStream()));
        this.out = new DataOutputStream(new BufferedOutputStream(socket.getOutputStream()));
    }

    public void send(Frame frame) throws IOException {
        send(frame.tag(), frame.type(), frame.data());
    }

    public void send(int tag, MsgType type, byte[] data) throws IOException {
        sendLock.lock();
        try {
            out.writeInt(tag);               // TAG
            out.writeInt(type.ordinal());    // TYPE (enum ordinal)
            out.writeInt(data.length);       // LENGTH
            out.write(data);                 // PAYLOAD
            out.flush();
        } finally {
            sendLock.unlock();
        }
    }

    public Frame receive() throws IOException {
        receiveLock.lock();
        try {
            int tag = in.readInt();

            int ordinal = in.readInt();
            MsgType type = MsgType.values()[ordinal];

            int len = in.readInt();
            byte[] data = new byte[len];
            in.readFully(data);

            return new Frame(tag, type, data);

        } finally {
            receiveLock.unlock();
        }
    }

    @Override
    public void close() throws IOException {
        socket.close();
    }
}
