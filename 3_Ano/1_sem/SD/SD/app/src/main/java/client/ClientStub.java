package client;

import protocol.*;
import java.io.*;
import java.net.Socket;
import java.util.*;
import java.util.concurrent.locks.ReentrantLock;

public class ClientStub {
    private Socket socket;
    private Demultiplexer demux;

    private int nextTag = 1;
    private final ReentrantLock tagLock = new ReentrantLock();

    private int newTag() {
        tagLock.lock();
        try {
            return nextTag++;
        } finally {
            tagLock.unlock();
        }
    }

    public ClientStub(String host, int port) throws IOException {
        this.socket = new Socket(host, port);
        this.demux = new Demultiplexer(new TaggedConnection(socket));
        this.demux.start();
    }

    public boolean logIn(String user, String pass) throws IOException, InterruptedException {
        int tag = newTag();
        byte[] payload = Protocol.serializeLoginRequest(user, pass);
        demux.send(tag, MsgType.AUTH, payload);

        Frame reply = demux.receive(tag);
        DataInputStream dis = new DataInputStream(new ByteArrayInputStream(reply.data()));

        boolean success = dis.readBoolean();

        return success;
    }

    public boolean signIn(String user, String pass) throws IOException, InterruptedException {
        int tag = newTag();
        byte[] payload = Protocol.serializeSignInRequest(user, pass);
        demux.send(tag, MsgType.AUTH, payload);

        Frame reply = demux.receive(tag);
        DataInputStream dis = new DataInputStream(new ByteArrayInputStream(reply.data()));

        boolean success = dis.readBoolean();

        return success;
    }

    public boolean insertEvent(String product, double quantity, double price)
            throws IOException, InterruptedException {
        int tag = newTag();
        byte[] payload = Protocol.serializeInsertRequest(product, quantity, price);
        demux.send(tag, MsgType.INSERT, payload);

        Frame reply = demux.receive(tag);
        DataInputStream dis = new DataInputStream(new ByteArrayInputStream(reply.data()));

        boolean success = dis.readBoolean();
        return success;
    }

    public String advanceDay() throws IOException, InterruptedException {
        int tag = newTag();
        byte[] payload = Protocol.serializeAdvanceDayRequest();
        demux.send(tag, MsgType.ADMIN, payload);

        Frame reply = demux.receive(tag);
        DataInputStream dis = new DataInputStream(new ByteArrayInputStream(reply.data()));

        boolean success = dis.readBoolean();
        String newDate = dis.readUTF();

        if (!success) {
            throw new IOException("Falha ao avançar o dia");
        }

        System.out.println("[ADVANCE] Novo dia: " + newDate);
        return newDate;
    }

    public String resetDay() throws IOException, InterruptedException {
        int tag = newTag();
        byte[] payload = Protocol.serializeResetDayRequest();
        demux.send(tag, MsgType.ADMIN, payload);

        Frame reply = demux.receive(tag);
        DataInputStream dis = new DataInputStream(new ByteArrayInputStream(reply.data()));

        boolean success = dis.readBoolean();
        String currentDate = dis.readUTF();

        if (!success) {
            throw new IOException("Falha ao resetar o dia");
        }

        System.out.println("[RESET] Data atual: " + currentDate);
        return currentDate;
    }

    public String getCurrentDay() throws IOException, InterruptedException {
        int tag = newTag();
        byte[] payload = Protocol.serializeCurrentDayRequest();
        demux.send(tag, MsgType.ADMIN, payload);

        Frame reply = demux.receive(tag);
        DataInputStream dis = new DataInputStream(new ByteArrayInputStream(reply.data()));

        String currentDate = dis.readUTF();

        return currentDate;
    }

    public double getQuantitySold(String product, int days)
            throws IOException, InterruptedException {
        return requestAggregation((byte) 0, product, days);
    }

    public double getSalesVolume(String product, int days)
            throws IOException, InterruptedException {
        return requestAggregation((byte) 1, product, days);
    }

    public double getAveragePrice(String product, int days)
            throws IOException, InterruptedException {
        return requestAggregation((byte) 2, product, days);
    }

    public double getMaxPrice(String product, int days)
            throws IOException, InterruptedException {
        return requestAggregation((byte) 3, product, days);
    }

    private double requestAggregation(byte aggType, String product, int days) throws IOException, InterruptedException {
        int tag = newTag();
        byte[] payload = Protocol.serializeAggregationRequest(aggType, product, days);
        demux.send(tag, MsgType.ADMIN, payload);

        Frame reply = demux.receive(tag);
        DataInputStream dis = new DataInputStream(new ByteArrayInputStream(reply.data()));

        double result = dis.readDouble();

        if (result == -1.0) {
            throw new IOException("Erro na agregação (parâmetros inválidos ou erro no servidor)");
        }

        return result;
    }

    public FilterResult filterEventsByDate(String date)
            throws IOException, InterruptedException {
        int tag = newTag();
        byte[] payload = Protocol.serializeFilterRequest(date);
        demux.send(tag, MsgType.FILTER, payload);

        Frame reply = demux.receive(tag);
        DataInputStream dis = new DataInputStream(new ByteArrayInputStream(reply.data()));

        int count = dis.readInt();
        List<Protocol.EventDTO> events = new ArrayList<>(count);

        for (int i = 0; i < count; i++) {
            String product = dis.readUTF();
            double quantity = dis.readDouble();
            double price = dis.readDouble();
            events.add(new Protocol.EventDTO(product, quantity, price));
        }

        double dayTotal = dis.readDouble();

        return new FilterResult(events, dayTotal);
    }

    public Thread waitForConsecutiveSales(int n, long timeoutMillis, NotificationCallback callback) {

        Thread notificationThread = new Thread(() -> {
            try {
                System.out.println("[NotificationThread] 🔔 Aguardando " + n + " vendas consecutivas...");

                int tag = newTag();
                byte[] payload = Protocol.serializeConsecutiveSalesRequest(n, timeoutMillis);
                demux.send(tag, MsgType.NOTIFY, payload);

                Frame reply = demux.receive(tag);
                DataInputStream dis = new DataInputStream(new ByteArrayInputStream(reply.data()));

                boolean happened = dis.readBoolean();
                String product = happened ? dis.readUTF() : null;

                if (callback != null) {
                    callback.onNotification(happened, product);
                }

                System.out.println("[NotificationThread] ✓ Notificação processada: success=" + happened +
                        ", product=" + product);

            } catch (InterruptedException e) {
                System.out.println("[NotificationThread] ⚠ Thread interrompida");
                if (callback != null) {
                    callback.onNotification(false, null);
                }
            } catch (Exception e) {
                System.err.println("[NotificationThread] ✗ Erro: " + e.getMessage());
                if (callback != null) {
                    callback.onNotification(false, null);
                }
            }
        }, "NotificationThread-Consecutive-" + System.currentTimeMillis());

        notificationThread.start();

        return notificationThread;
    }

    public Thread waitForSimultaneousSales(String p1, String p2, long timeoutMillis, NotificationCallback callback) {

        Thread notificationThread = new Thread(() -> {
            try {
                System.out.println("[NotificationThread] 🔔 Aguardando vendas simultâneas: " + p1 + " e " + p2);

                int tag = newTag();
                byte[] payload = Protocol.serializeSimultaneousSalesRequest(p1, p2, timeoutMillis);
                demux.send(tag, MsgType.NOTIFY, payload);

                Frame reply = demux.receive(tag);
                DataInputStream dis = new DataInputStream(new ByteArrayInputStream(reply.data()));

                boolean happened = dis.readBoolean();

                if (callback != null) {
                    callback.onNotification(happened, happened ? p1 + " e " + p2 : null);
                }

                System.out.println("[NotificationThread] ✓ Notificação processada: success=" + happened);

            } catch (InterruptedException e) {
                System.out.println("[NotificationThread] ⚠ Thread interrompida");
                if (callback != null) {
                    callback.onNotification(false, null);
                }
            } catch (Exception e) {
                System.err.println("[NotificationThread] ✗ Erro: " + e.getMessage());
                if (callback != null) {
                    callback.onNotification(false, null);
                }
            }
        }, "NotificationThread-Simultaneous-" + System.currentTimeMillis());

        notificationThread.start();

        return notificationThread;
    }

    public void close() throws IOException {
        demux.close();
        socket.close();
    }

    public static class FilterResult {
        public final List<Protocol.EventDTO> events;
        public final double dayTotal;

        public FilterResult(List<Protocol.EventDTO> events, double dayTotal) {
            this.events = events;
            this.dayTotal = dayTotal;
        }

        public void print() {
            System.out.println("Eventos encontrados: " + events.size());
            for (Protocol.EventDTO e : events) {
                System.out.printf("  - %s: %.2f unidades @ %.2f€ = %.2f€\n",
                        e.product(), e.quantity(), e.price(), e.quantity() * e.price());
            }
            System.out.printf("Total do dia: %.2f€\n", dayTotal);
        }
    }
}
