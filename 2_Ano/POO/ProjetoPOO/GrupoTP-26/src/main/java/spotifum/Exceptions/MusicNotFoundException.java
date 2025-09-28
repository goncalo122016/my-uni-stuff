package spotifum.Exceptions;

public class MusicNotFoundException extends RuntimeException {
    public MusicNotFoundException(String msg) {
        super(msg);
    }
}