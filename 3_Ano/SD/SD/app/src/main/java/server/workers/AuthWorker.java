package server.workers;

import protocol.MsgType;
import protocol.OpCode;
import protocol.Protocol;
import server.AuthService;
import protocol.Frame;
import protocol.TaggedConnection;
import java.io.*;

public final class AuthWorker {
    private AuthWorker() {
    }

    public static void handle(Frame frame,
            TaggedConnection conn,
            AuthService authService) {
        try {
            byte[] data = frame.data();
            int tag = frame.tag();

            DataInputStream dis = new DataInputStream(new ByteArrayInputStream(data));
            byte opByte = dis.readByte();
            OpCode op = OpCode.fromCode(opByte);

            byte[] respBytes;

            if (op == OpCode.LOGIN) {
                String user = dis.readUTF();
                String pass = dis.readUTF();
                boolean ok = authService.authenticate(user, pass);
                respBytes = Protocol.serializeLoginResponse(ok);
            } else if (op == OpCode.SIGNIN) {
                String user = dis.readUTF();
                String pass = dis.readUTF();
                boolean ok = authService.register(user, pass);
                respBytes = Protocol.serializeSignInResponse(ok);
            } else {
                throw new IOException("OpCode AUTH inválido: " + op);
            }

            conn.send(tag, MsgType.AUTH, respBytes);

        } catch (Exception e) {
            System.out.println("[AuthTask] erro: " + e);
        }
    }
}
