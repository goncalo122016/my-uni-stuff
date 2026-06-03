import java.util.Set;

public interface IManager {
    String begin(Set<String> keySet) throws InterruptedException;
    void commit(String id);
    char getTransactionStatus(String id);
}
