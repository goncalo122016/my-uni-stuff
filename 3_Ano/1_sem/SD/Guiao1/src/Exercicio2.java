import java.util.concurrent.locks.Lock;
import java.util.concurrent.locks.ReentrantLock;

class Bank extends ReentrantLock {

    private static class Account {
        private int balance;
        Account(int balance) { this.balance = balance; }
        int balance() { return balance; }
        boolean deposit(int value) {
            balance += value;
            return true;
        }
    }

    // Our single account, for now
    private Account savings = new Account(0);
    Lock l = new ReentrantLock();

    // Account balance
    public int balance() {
        return savings.balance();
    }

    // Deposit
    boolean deposit(int value) {
        l.lock();
        boolean r = savings.deposit(value);
        l.unlock();
        return r;
    }
}

class DepositMany implements Runnable {
    int value;
    int deposits;
    Bank bank;
    DepositMany(int I, int V, Bank bank) {
        this.deposits = I;
        this.value = V;
        this.bank = bank;
    }

    @Override
    public void run() {
        for (int i = 0; i < deposits; i++) {
            bank.deposit(value);
        }
    }
}

public class Exercicio2 {
    public static void main(String[] args) {
        Bank b =  new Bank();
        int N = 10;
        int I = 1000;
        int V = 1;
        Thread[] threads = new Thread[N];

        System.out.println("Saldo inicial: " + b.balance());

        for (int i = 0; i < N; i++) {
            threads[i] = new Thread (new DepositMany(I, V, b));
            threads[i].start();
        }

        try  {
            for (int i = 0; i < N; i++) {
                threads[i].join();
            }
        }
        catch (InterruptedException e) {
            throw new RuntimeException(e);
        }

        System.out.println("Saldo final: " + b.balance());
    }
}