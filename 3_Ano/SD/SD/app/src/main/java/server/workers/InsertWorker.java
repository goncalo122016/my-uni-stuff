package server.workers;

import protocol.MsgType;
import protocol.Protocol;
import protocol.TaggedConnection;
import server.event.EventService;
import protocol.Frame;

import java.io.*;

public final class InsertWorker {
    private InsertWorker() {
    }

    public static void handle(Frame frame,
            TaggedConnection conn,
            EventService eventService) {
        try {
            byte[] data = frame.data();
            int tag = frame.tag();

            ByteArrayInputStream bis = new ByteArrayInputStream(data);
            DataInputStream dis = new DataInputStream(bis);

            String productName = dis.readUTF();
            double quantity = dis.readDouble();
            double price = dis.readDouble();

            boolean ok;

            ok = eventService.addEvent(productName, quantity, price);

            byte[] respBytes = Protocol.serializeInsertResponse(ok);

            conn.send(tag, MsgType.INSERT, respBytes);
        } catch (Exception e) {
            System.out.println("[InsertWorker] worker terminou: " + e);
        }
    }
}
