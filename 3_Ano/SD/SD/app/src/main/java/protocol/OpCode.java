package protocol;

public enum OpCode {
    LOGIN(0),
    SIGNIN(1),
    ADVANCE(2),
    RESET(3),
    CURRENT_DAY(4),
    AGGREGATION(5),
    SIMULATNEOUS_SALES(6),
    CONSECUTIVE_SALES(7);


    private final int code;

    OpCode(int c) {
        this.code = c;
    }

    public int code() {
        return code;
    }

    public static OpCode fromCode(int c) {
        return switch (c) {
            case 0 -> LOGIN;
            case 1 -> SIGNIN;
            case 2 -> ADVANCE;
            case 3 -> RESET;
            case 4 -> CURRENT_DAY;
            case 5 -> AGGREGATION;
            case 6 -> SIMULATNEOUS_SALES;
            case 7 -> CONSECUTIVE_SALES;
            default -> throw new IllegalArgumentException("Invalid OpCode: " + c);
        };
    }

}
