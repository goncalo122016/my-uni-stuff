class Increment implements Runnable {

    @Override
    public void run() {
        final long I=100;
        for (long i = 0; i < I; i++)
            System.out.println("Thread: " + Thread.currentThread().threadId() + " - " + i);
    }
}

public class Exercício1 {
    public static void main(String[] args) {
        final int N = 10;
        Thread[] threads = new Thread[N];
        for (int i = 0; i < N; i++) {
            threads[i] = new Thread (new Increment());
            threads[i].start();
        }

        try{
            for (int i = 0; i < N; i++) {
                threads[i].join();
            }
            System.out.println("fim");
        }
        catch (InterruptedException e){
            throw new RuntimeException(e);
        }
    }
}
