package server.workers;

import protocol.MsgType;
import protocol.OpCode;
import protocol.Protocol;
import protocol.TaggedConnection;
import server.NotificationService;
import protocol.Frame;

import java.io.*;

public final class NotificationWorker {
    private NotificationWorker() {
    }

    public static void handle(Frame frame,
            TaggedConnection conn,
            NotificationService notificationService) {
        try {
            byte[] data = frame.data();
            int tag = frame.tag();

            ByteArrayInputStream bis = new ByteArrayInputStream(data);
            DataInputStream dis = new DataInputStream(bis);

            byte opCodeByte = dis.readByte();
            OpCode opCode = OpCode.fromCode(opCodeByte);

            byte[] respBytes;

            switch (opCode) {
                case SIMULATNEOUS_SALES -> {
                    String p1 = dis.readUTF();
                    String p2 = dis.readUTF();
                    long time = dis.readLong();

                    boolean result = notificationService.waitForSimultaneousSales(p1, p2, time);

                    respBytes = Protocol.serializeSimultaneousSalesResponse(result);
                }
                case CONSECUTIVE_SALES -> {
                    int n = dis.readInt();
                    long time = dis.readLong();

                    String product = notificationService.waitForConsecutiveSales(n, time);
                    boolean happened = (product != null);

                    respBytes = Protocol.serializeConsecutiveSalesResponse(happened, product);
                }
                default -> throw new IOException("OpCode NOTIFY inválido: " + opCode);
            }

            conn.send(tag, MsgType.NOTIFY, respBytes);
        } catch (Exception e) {
            System.out.println("[NotificationWorker] worker terminou: " + e);
        }
    }
}
