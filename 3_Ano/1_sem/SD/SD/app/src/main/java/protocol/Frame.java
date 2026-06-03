package protocol;

public record Frame(int tag, MsgType type, byte[] data) {}