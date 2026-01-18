import java.util.Set;
import java.util.concurrent.locks.Condition;

class Transaction {
    final String id;
    char status;              // 'b', 'e', 'c'
    final Set<String> keys;   // null => transação de manutenção
    final Condition cond;

    Transaction(String id, Set<String> keys, Condition cond) {
        this.id = id;
        this.keys = keys;
        this.cond = cond;
        this.status = 'b';
    }

    boolean conflictsWith(Transaction other) {
        if (this.status == 'c' || other.status == 'c') return false;
        if (this.keys == null || other.keys == null) return true; // manutenção
        for (String k : keys)
            if (other.keys.contains(k))
                return true;
        return false;
    }
}
