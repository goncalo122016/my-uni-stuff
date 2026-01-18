package protocol;

import java.io.*;
import java.util.ArrayList;
import java.util.List;

public class Protocol {

    public record LoginRequest(String user, String pass) {}
    public record LoginResponse(boolean ok) {}

    public record SignInRequest(String user, String pass) {}
    public record SignInResponse(boolean ok) {}

    public static byte[] serializeLoginRequest(String user, String pass) throws IOException {
        ByteArrayOutputStream bos = new ByteArrayOutputStream();
        DataOutputStream dos = new DataOutputStream(bos);

        dos.writeByte(OpCode.LOGIN.code());
        dos.writeUTF(user);
        dos.writeUTF(pass);

        dos.flush();
        return bos.toByteArray();
    }

    public static byte[] serializeSignInRequest(String user, String pass) throws IOException {
        ByteArrayOutputStream bos = new ByteArrayOutputStream();
        DataOutputStream dos = new DataOutputStream(bos);

        dos.writeByte(OpCode.SIGNIN.code());
        dos.writeUTF(user);
        dos.writeUTF(pass);

        dos.flush();
        return bos.toByteArray();
    }

    public static LoginRequest deserializeLoginRequest(byte[] data) throws IOException {
        DataInputStream dis = new DataInputStream(new ByteArrayInputStream(data));
        String user = dis.readUTF();
        String pass = dis.readUTF();
        return new LoginRequest(user, pass);
    }

    public static SignInRequest deserializeSignInRequest(byte[] data) throws IOException {
        DataInputStream dis = new DataInputStream(new ByteArrayInputStream(data));
        String user = dis.readUTF();
        String pass = dis.readUTF();
        return new SignInRequest(user, pass);
    }

    public static byte[] serializeLoginResponse(boolean ok) throws IOException {
        ByteArrayOutputStream bos = new ByteArrayOutputStream();
        DataOutputStream dos = new DataOutputStream(bos);

        dos.writeBoolean(ok);

        dos.flush();
        return bos.toByteArray();
    }

    public static byte[] serializeSignInResponse(boolean ok) throws IOException {
        ByteArrayOutputStream bos = new ByteArrayOutputStream();
        DataOutputStream dos = new DataOutputStream(bos);

        dos.writeBoolean(ok);

        dos.flush();
        return bos.toByteArray();
    }

    public static LoginResponse deserializeLoginResponse(byte[] data) throws IOException {
        DataInputStream dis = new DataInputStream(new ByteArrayInputStream(data));
        boolean ok = dis.readBoolean();
        return new LoginResponse(ok);
    }

    public static SignInResponse deserializeSignInResponse(byte[] data) throws IOException {
        DataInputStream dis = new DataInputStream(new ByteArrayInputStream(data));
        boolean ok = dis.readBoolean();
        return new SignInResponse(ok);
    }


    public record InsertRequest(String product, double quantity, double price) {}
    public record InsertResponse(boolean ok) {}

    public static byte[] serializeInsertRequest(String product, double quantity, double price)
            throws IOException {
        ByteArrayOutputStream bos = new ByteArrayOutputStream();
        DataOutputStream dos = new DataOutputStream(bos);

        dos.writeUTF(product);
        dos.writeDouble(quantity);
        dos.writeDouble(price);

        dos.flush();
        return bos.toByteArray();
    }

    public static InsertRequest deserializeInsertRequest(byte[] data) throws IOException {
        DataInputStream dis = new DataInputStream(new ByteArrayInputStream(data));

        String productName = dis.readUTF();
        double quantity = dis.readDouble();
        double price = dis.readDouble();

        return new InsertRequest(productName, quantity, price);
    }

    public static byte[] serializeInsertResponse(boolean ok) throws IOException {
        ByteArrayOutputStream bos = new ByteArrayOutputStream();
        DataOutputStream dos = new DataOutputStream(bos);

        dos.writeBoolean(ok);

        dos.flush();
        return bos.toByteArray();
    }

    public static InsertResponse deserializeInsertResponse(byte[] data) throws IOException {
        DataInputStream dis = new DataInputStream(new ByteArrayInputStream(data));

        boolean ok = dis.readBoolean();

        return new InsertResponse(ok);
    }


    public record AdvanceDayRequest() {}
    public record ResetDayRequest() {}
    public record CurrentDayRequest() {}
    public record AggregationRequest(byte aggType, String product, int days) {}

    public record AdvanceDayResponse(boolean ok, String currentDate) {}
    public record ResetDayResponse(boolean ok, String currentDate) {}
    public record CurrentDayResponse(String currentDate, long offsetDays) {}
    public record AggregationResponse(double result) {}


    public static byte[] serializeAdvanceDayRequest() throws IOException {
        ByteArrayOutputStream bos = new ByteArrayOutputStream();
        DataOutputStream dos = new DataOutputStream(bos);

        dos.writeByte(OpCode.ADVANCE.code());
        dos.flush();
        return bos.toByteArray();
    }

    public static byte[] serializeResetDayRequest() throws IOException {
        ByteArrayOutputStream bos = new ByteArrayOutputStream();
        DataOutputStream dos = new DataOutputStream(bos);

        dos.writeByte(OpCode.RESET.code());
        dos.flush();
        return bos.toByteArray();
    }

    public static byte[] serializeCurrentDayRequest() throws IOException {
        ByteArrayOutputStream bos = new ByteArrayOutputStream();
        DataOutputStream dos = new DataOutputStream(bos);

        dos.writeByte(OpCode.CURRENT_DAY.code());
        dos.flush();
        return bos.toByteArray();
    }

    public static byte[] serializeAggregationRequest(byte aggType, String product, int days)
            throws IOException {
        ByteArrayOutputStream bos = new ByteArrayOutputStream();
        DataOutputStream dos = new DataOutputStream(bos);

        dos.writeByte(OpCode.AGGREGATION.code());
        dos.writeByte(aggType);
        dos.writeUTF(product);
        dos.writeInt(days);

        dos.flush();
        return bos.toByteArray();
    }

    public static AdvanceDayRequest deserializeAdvanceDayRequest(byte[] data) {
        return new AdvanceDayRequest();
    }

    public static ResetDayRequest deserializeResetDayRequest(byte[] data) {
        return new ResetDayRequest();
    }

    public static CurrentDayRequest deserializeCurrentDayRequest(byte[] data) {
        return new CurrentDayRequest();
    }

    public static AggregationRequest deserializeAggregationRequest(byte[] data) throws IOException {
        DataInputStream dis = new DataInputStream(new ByteArrayInputStream(data));

        byte aggType = dis.readByte();
        String product = dis.readUTF();
        int days = dis.readInt();

        return new AggregationRequest(aggType, product, days);
    }


    public static byte[] serializeAdvanceDayResponse(boolean ok, String date) throws IOException {
        ByteArrayOutputStream bos = new ByteArrayOutputStream();
        DataOutputStream dos = new DataOutputStream(bos);

        dos.writeBoolean(ok);
        dos.writeUTF(date);

        dos.flush();
        return bos.toByteArray();
    }

    public static byte[] serializeResetDayResponse(boolean ok, String date) throws IOException {
        ByteArrayOutputStream bos = new ByteArrayOutputStream();
        DataOutputStream dos = new DataOutputStream(bos);

        dos.writeBoolean(ok);
        dos.writeUTF(date);

        dos.flush();
        return bos.toByteArray();
    }

    public static byte[] serializeCurrentDayResponse(String date, long offset) throws IOException {
        ByteArrayOutputStream bos = new ByteArrayOutputStream();
        DataOutputStream dos = new DataOutputStream(bos);

        dos.writeUTF(date);
        dos.writeLong(offset);

        dos.flush();
        return bos.toByteArray();
    }

    public static byte[] serializeAggregationResponse(double result) throws IOException {
        ByteArrayOutputStream bos = new ByteArrayOutputStream();
        DataOutputStream dos = new DataOutputStream(bos);

        //dos.writeByte(status);
        dos.writeDouble(result);

        dos.flush();
        return bos.toByteArray();
    }

    public static AdvanceDayResponse deserializeAdvanceDayResponse(byte[] data) throws IOException {
        DataInputStream dis = new DataInputStream(new ByteArrayInputStream(data));
        boolean ok = dis.readBoolean();
        String date = dis.readUTF();
        return new AdvanceDayResponse(ok, date);
    }

    public static ResetDayResponse deserializeResetDayResponse(byte[] data) throws IOException {
        DataInputStream dis = new DataInputStream(new ByteArrayInputStream(data));
        boolean ok = dis.readBoolean();
        String date = dis.readUTF();
        return new ResetDayResponse(ok, date);
    }

    public static CurrentDayResponse deserializeCurrentDayResponse(byte[] data) throws IOException {
        DataInputStream dis = new DataInputStream(new ByteArrayInputStream(data));
        String date = dis.readUTF();
        long offset = dis.readLong();
        return new CurrentDayResponse(date, offset);
    }

    public static AggregationResponse deserializeAggregationResponse(byte[] data) throws IOException {
        DataInputStream dis = new DataInputStream(new ByteArrayInputStream(data));
        //byte status = dis.readByte();
        double res = dis.readDouble();
        return new AggregationResponse(res);
    }


    public record FilterRequest(String date) {}
    public record EventDTO(String product, double quantity, double price) {}
    public record FilterResponse(List<EventDTO> events, double dayTotal) {}

    public static byte[] serializeFilterRequest(String date) throws IOException {
        ByteArrayOutputStream bos = new ByteArrayOutputStream();
        DataOutputStream dos = new DataOutputStream(bos);

        dos.writeUTF(date);

        dos.flush();
        return bos.toByteArray();
    }

    public static FilterRequest deserializeFilterRequest(byte[] data) throws IOException {
        DataInputStream dis = new DataInputStream(new ByteArrayInputStream(data));
        String date = dis.readUTF();
        return new FilterRequest(date);
    }

    public static byte[] serializeFilterResponse(List<EventDTO> events, double dayTotal)
            throws IOException {
        ByteArrayOutputStream bos = new ByteArrayOutputStream();
        DataOutputStream dos = new DataOutputStream(bos);

        dos.writeInt(events.size());
        for (EventDTO e : events) {
            dos.writeUTF(e.product());
            dos.writeDouble(e.quantity());
            dos.writeDouble(e.price());
        }
        dos.writeDouble(dayTotal);

        dos.flush();
        return bos.toByteArray();
    }

    public static FilterResponse deserializeFilterResponse(byte[] data) throws IOException {
        DataInputStream dis = new DataInputStream(new ByteArrayInputStream(data));

        int size = dis.readInt();
        List<EventDTO> events = new ArrayList<>(size);
        for (int i = 0; i < size; i++) {
            String product = dis.readUTF();
            double quantity = dis.readDouble();
            double price = dis.readDouble();
            events.add(new EventDTO(product, quantity, price));
        }
        double dayTotal = dis.readDouble();

        return new FilterResponse(events, dayTotal);
    }

    public record SimultaneousSalesRequest(String product1, String product2, long timeoutMillis) {}
    public record SimultaneousSalesResponse(boolean happened) {}

    public record ConsecutiveSalesRequest(int n, long timeoutMillis) {}
    public record ConsecutiveSalesResponse(boolean happened, String product) {}


    public static byte[] serializeSimultaneousSalesRequest(String p1, String p2, long timeoutMillis)
            throws IOException {
        ByteArrayOutputStream bos = new ByteArrayOutputStream();
        DataOutputStream dos = new DataOutputStream(bos);

        dos.writeByte(OpCode.SIMULATNEOUS_SALES.code());
        dos.writeUTF(p1);
        dos.writeUTF(p2);
        dos.writeLong(timeoutMillis);

        dos.flush();
        return bos.toByteArray();
    }

    public static SimultaneousSalesRequest deserializeSimultaneousSalesRequest(byte[] data)
            throws IOException {
        DataInputStream dis = new DataInputStream(new ByteArrayInputStream(data));

        String p1 = dis.readUTF();
        String p2 = dis.readUTF();
        long timeoutMillis = dis.readLong();

        return new SimultaneousSalesRequest(p1, p2, timeoutMillis);
    }

    public static byte[] serializeConsecutiveSalesRequest(int n, long timeoutMillis)
            throws IOException {
        ByteArrayOutputStream bos = new ByteArrayOutputStream();
        DataOutputStream dos = new DataOutputStream(bos);

        dos.writeByte(OpCode.CONSECUTIVE_SALES.code());
        dos.writeInt(n);
        dos.writeLong(timeoutMillis);

        dos.flush();
        return bos.toByteArray();
    }

    public static ConsecutiveSalesRequest deserializeConsecutiveSalesRequest(byte[] data)
            throws IOException {
        DataInputStream dis = new DataInputStream(new ByteArrayInputStream(data));

        int n = dis.readInt();
        long timeoutMillis = dis.readLong();

        return new ConsecutiveSalesRequest(n, timeoutMillis);
    }

    public static SimultaneousSalesResponse deserializeSimultaneousSalesResponse(byte[] data)
            throws IOException {
        DataInputStream dis = new DataInputStream(new ByteArrayInputStream(data));
        boolean happened = dis.readBoolean();
        return new SimultaneousSalesResponse(happened);
    }

    public static byte[] serializeSimultaneousSalesResponse(boolean happened) throws IOException {
        ByteArrayOutputStream bos = new ByteArrayOutputStream();
        DataOutputStream dos = new DataOutputStream(bos);

        dos.writeBoolean(happened);

        dos.flush();
        return bos.toByteArray();
    }

    public static ConsecutiveSalesResponse deserializeConsecutiveSalesResponse(byte[] data)
            throws IOException {
        DataInputStream dis = new DataInputStream(new ByteArrayInputStream(data));

        boolean happened = dis.readBoolean();
        String product = null;
        if (happened) product = dis.readUTF();

        return new ConsecutiveSalesResponse(happened, product);
    }

    public static byte[] serializeConsecutiveSalesResponse(boolean happened, String product)
            throws IOException {
        ByteArrayOutputStream bos = new ByteArrayOutputStream();
        DataOutputStream dos = new DataOutputStream(bos);

        dos.writeBoolean(happened);
        if (happened) dos.writeUTF(product);

        dos.flush();
        return bos.toByteArray();
    }
}
