package server;

import protocol.*;
import server.event.EventService;
import java.io.*;
import java.net.Socket;

public class AdminHandler implements Runnable {
    private final Socket socket;
    private final EventService eventService;
    private final AggregationService aggregationService;
    private final int D;
    private TaggedConnection conn;

    public AdminHandler(Socket socket, EventService eventService, AggregationService aggregationService, int D) {
        this.socket = socket;
        this.eventService = eventService;
        this.aggregationService = aggregationService;
        this.D = D;
    }

    @Override
    public void run() {
        try {
            this.conn = new TaggedConnection(socket);
            System.out.println("[AdminHandler] Sessão iniciada: " + socket.getInetAddress());

            while (true) {
                Frame frame = conn.receive();

                System.out.println("[AdminHandler] Frame: tag=" + frame.tag() + ", type=" + frame.type());

                if (frame.type() != MsgType.ADMIN) {
                    System.err.println("[AdminHandler] Tipo inválido: " + frame.type());
                    continue;
                }

                handleAdminRequest(frame);
            }

        } catch (IOException e) {
            System.out.println("[AdminHandler] Sessão encerrada: " + e.getMessage());
        } finally {
            try {
                if (conn != null) conn.close();
                socket.close();
            } catch (IOException ignored) {}
        }
    }

    private void handleAdminRequest(Frame frame) throws IOException {
        byte[] data = frame.data();
        int tag = frame.tag();

        DataInputStream dis = new DataInputStream(new ByteArrayInputStream(data));
        byte opByte = dis.readByte();
        OpCode op = OpCode.fromCode(opByte);

        System.out.println("[AdminHandler] OpCode: " + op);

        byte[] respBytes;

        try {
            switch (op) {
                case ADVANCE -> {
                    eventService.advanceDay();
                    aggregationService.invalidateCache();
                    String newDate = eventService.getCurrentSimulatedDate();

                    System.out.println("[AdminHandler] Dia avançado: " + newDate);
                    respBytes = Protocol.serializeAdvanceDayResponse(true, newDate);
                }

                case RESET -> {
                    eventService.resetToToday();
                    aggregationService.invalidateCache();
                    String currentDate = eventService.getCurrentSimulatedDate();

                    System.out.println("[AdminHandler] Reset: " + currentDate);
                    respBytes = Protocol.serializeResetDayResponse(true, currentDate);
                }

                case CURRENT_DAY -> {
                    String currentDate = eventService.getCurrentSimulatedDate();
                    long offset = eventService.getDayOffset();

                    System.out.println("[AdminHandler] Data: " + currentDate + ", offset=" + offset);
                    respBytes = Protocol.serializeCurrentDayResponse(currentDate, offset);
                }

                case AGGREGATION -> {
                    byte aggType = dis.readByte();
                    String product = dis.readUTF();
                    int days = dis.readInt();

                    System.out.println("[AdminHandler] Agregação: type=" + aggType +
                                      ", product=" + product + ", days=" + days);

                    if (days < 1 || days > D) {
                        System.err.println("[AdminHandler] Dias inválido: " + days);
                        respBytes = Protocol.serializeAggregationResponse( 0.0);
                    } else {
                        double result = switch (aggType) {
                            case 0 -> aggregationService.getQuantitySold(product, days);
                            case 1 -> aggregationService.getSalesVolume(product, days);
                            case 2 -> aggregationService.getAveragePrice(product, days);
                            case 3 -> aggregationService.getMaxPrice(product, days);
                            default -> throw new IOException("Tipo inválido: " + aggType);
                        };

                        System.out.println("[AdminHandler] Resultado: " + result);
                        respBytes = Protocol.serializeAggregationResponse(result);
                    }
                }

                default -> throw new IOException("OpCode inválido: " + op);
            }

            conn.send(tag, MsgType.ADMIN, respBytes);

        } catch (Exception e) {
            System.err.println("[AdminHandler] Erro: " + e.getMessage());
            e.printStackTrace();
        }
    }
}
