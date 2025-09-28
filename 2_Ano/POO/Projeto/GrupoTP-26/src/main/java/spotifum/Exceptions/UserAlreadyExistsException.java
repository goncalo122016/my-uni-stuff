package spotifum.Exceptions;

public class UserAlreadyExistsException extends RuntimeException {
  public UserAlreadyExistsException(String msg) {
    super(msg);
  }
}