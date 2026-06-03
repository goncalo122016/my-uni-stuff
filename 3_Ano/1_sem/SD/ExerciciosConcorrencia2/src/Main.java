import java.time.LocalTime;
import java.time.format.DateTimeFormatter;
import java.util.concurrent.ExecutorService;
import java.util.concurrent.Executors;
import java.util.concurrent.TimeUnit;

public class Main {
    private static final DateTimeFormatter TS = DateTimeFormatter.ofPattern("HH:mm:ss.SSS");

    private static String ts() {
        return LocalTime.now().format(TS);
    }

    private static Runnable makePlayer(Manager manager, String name, int minPlayers, long delayMs) {
        return () -> {
            try {
                Thread.sleep(delayMs);
                System.out.printf("%s [ENTER] %s wants raid with min=%d%n", ts(), name, minPlayers);
                System.out.printf("%s [WAIT-GROUP] %s awaiting group formation%n", ts(), name);
                Raid raid = manager.join(name, minPlayers);
                //System.out.printf("%s [GROUPED] %s joined raid players=%s%n", ts(), name, raid.players());

                System.out.printf("%s [WAIT-START] %s awaiting raid start%n", ts(), name);
                raid.waitStart();
                System.out.printf("%s [STARTED] %s raid started%n", ts(), name);

                Thread.sleep(500 + (long) (Math.random() * 1000));

                System.out.printf("%s [LEAVE] %s leaving raid %n", ts(), name);
                raid.leave();
                //System.out.printf("%s [DONE] %s completed%n", ts(), name);
            } catch (InterruptedException e) {
                System.out.printf("%s [INTERRUPTED] %s%n", ts(), name);
                Thread.currentThread().interrupt();
            } catch (Exception e) {
                System.out.printf("%s [ERROR] %s: %s%n", ts(), name, e.getMessage());
            }
        };
    }

    public static void main(String[] args) throws InterruptedException {
        Manager manager = new Manager();
        ExecutorService pool = Executors.newFixedThreadPool(8);

        pool.execute(makePlayer(manager, "Alice", 3, 0));
        pool.execute(makePlayer(manager, "Bob", 2, 100));
        pool.execute(makePlayer(manager, "Carol", 3, 200));
        pool.execute(makePlayer(manager, "Dave", 3, 300));
        pool.execute(makePlayer(manager, "Eve", 2, 800));
        pool.execute(makePlayer(manager, "Frank", 2, 900));
        pool.execute(makePlayer(manager, "Grace", 2, 1200));
        pool.execute(makePlayer(manager, "Heidi", 4, 1300));

        pool.shutdown();
        pool.awaitTermination(30, TimeUnit.SECONDS);
        System.out.printf("%s [TEST-END] All player tasks finished%n", ts());
    }
}
