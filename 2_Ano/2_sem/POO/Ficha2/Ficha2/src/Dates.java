import java.time.LocalDate;

public class Dates {
    LocalDate[] dates;
    int used;

    public void initDates(int size) {
        dates = new LocalDate[size];
        used = 0;
    }

    public void insertDate(LocalDate date) {
        if (used == dates.length) return;
        dates[used++] = date;
    }

    public void printDates() {
        for (int i = 0; i < used; i++) {
            System.out.println(dates[i]);
        }
    }
}
