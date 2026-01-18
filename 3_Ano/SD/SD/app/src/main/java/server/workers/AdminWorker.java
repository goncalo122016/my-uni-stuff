package server.workers;

import protocol.MsgType;
import protocol.OpCode;
import protocol.Protocol;
import server.event.EventService;
import server.AggregationService;
import protocol.Frame;
import protocol.TaggedConnection;

import java.io.*;

public final class AdminWorker {
    private AdminWorker() {

    }

    public static void handle(Frame frame,
            TaggedConnection conn,
            EventService eventService,
            AggregationService aggregationService) {
        try {
            byte[] data = frame.data();
            int tag = frame.tag();

            ByteArrayInputStream bis = new ByteArrayInputStream(data);
            DataInputStream dis = new DataInputStream(bis);

            byte opCodeByte = dis.readByte();
            OpCode opCode = OpCode.fromCode(opCodeByte);

            byte[] respBytes;

            switch (opCode) {
                case ADVANCE -> {
                    eventService.advanceDay();
                    aggregationService.invalidateCache();
                    String date = eventService.getCurrentSimulatedDate();

                    respBytes = Protocol.serializeAdvanceDayResponse(true, date);
                }
                case RESET -> {
                    eventService.resetToToday();
                    aggregationService.invalidateCache();
                    String date = eventService.getCurrentSimulatedDate();

                    respBytes = Protocol.serializeResetDayResponse(true, date);
                }
                case CURRENT_DAY -> {
                    String currentDate = eventService.getCurrentSimulatedDate();
                    long offset = eventService.getDayOffset();

                    respBytes = Protocol.serializeCurrentDayResponse(currentDate, offset);
                }
                case AGGREGATION -> {
                    byte aggType = dis.readByte();
                    String product = dis.readUTF();
                    int days = dis.readInt();

                    try {
                        double result = switch (aggType) {
                            case 0 -> aggregationService.getQuantitySold(product, days);
                            case 1 -> aggregationService.getSalesVolume(product, days);
                            case 2 -> aggregationService.getAveragePrice(product, days);
                            case 3 -> aggregationService.getMaxPrice(product, days);
                            default -> throw new IOException("Aggregation type inválido: " + aggType);
                        };
                        System.out.println(result);
                        respBytes = Protocol.serializeAggregationResponse(result);
                    } catch (IllegalArgumentException e) {
                        System.err.println("[AdminHandler] Parâmetros inválidos para agregação: " + e.getMessage());
                        respBytes = Protocol.serializeAggregationResponse(-1.0);
                    }
                }
                default -> throw new IOException("Operação ADMIN inválida: " + opCode);
            }

            conn.send(tag, MsgType.ADMIN, respBytes);
        } catch (Exception e) {
            System.out.println("[AdminWorker] worker terminou: " + e);
        }
    }
}