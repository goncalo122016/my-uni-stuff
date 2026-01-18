class HelloWorld implements Runnable {
    @Override
    public void run() {
        System.out.printf("Hello World from thread " + Thread.currentThread().threadId() + ", running on process " + ProcessHandle.current().pid() + "(within Runnable Instance)!\n");
        try {
            Thread.sleep(1000);
        } catch (InterruptedException e) {
            throw new RuntimeException(e);
        }
    }
}

public class Main {
    public static void main(String[] args) {
        //TIP Press <shortcut actionId="ShowIntentionActions"/> with your caret at the highlighted text
        // to see how IntelliJ IDEA suggests fixing it.
        System.out.printf("Hello World from thread " + Thread.currentThread().threadId() + ", running on process " + ProcessHandle.current().pid() + "!\n");
        Thread t1 = new Thread(new HelloWorld());
        Thread t2 = new Thread(new HelloWorld());
        t1.start();
        t2.start();

        try {
            t1.join();
            t2.join();
        } catch (InterruptedException e) {
            throw new RuntimeException(e);
        }
    }
}