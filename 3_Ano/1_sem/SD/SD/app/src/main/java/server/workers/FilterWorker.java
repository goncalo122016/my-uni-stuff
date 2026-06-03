package server.workers;

import protocol.MsgType;
import protocol.Protocol;
import protocol.TaggedConnection;
import server.event.EventService;
import protocol.Frame;
import server.event.Event;

import java.util.ArrayList;
import java.util.List;
import java.io.*;

public final class FilterWorker {
    private FilterWorker() {
    }

    public static void handle(Frame frame,
            TaggedConnection conn,
            EventService eventService) {
        try {
            byte[] data = frame.data();
            int tag = frame.tag();

            DataInputStream dis = new DataInputStream(new ByteArrayInputStream(data));

            String date = dis.readUTF();

            List<Event> events = eventService.getEventsByDate(date);

            double dayTotal = 0.0;
            List<Protocol.EventDTO> dtos = new ArrayList<>(events.size());
            for (Event e : events) {
                dtos.add(new Protocol.EventDTO(
                        e.getProductName(),
                        e.getQuantity(),
                        e.getPrice()));
                dayTotal += e.getQuantity() * e.getPrice();
            }

            byte[] respBytes = Protocol.serializeFilterResponse(dtos, dayTotal);

            conn.send(tag, MsgType.FILTER, respBytes);
        } catch (Exception e) {
            System.out.println("[FilterWorker] terminou: " + e);
        }

    }
}
