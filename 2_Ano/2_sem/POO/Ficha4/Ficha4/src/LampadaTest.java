import org.junit.jupiter.api.Test;
import static org.junit.jupiter.api.Assertions.*;

public class LampadaTest {

    @Test
    public void shouldHaveModeOff() {
        Lampada lampada = new Lampada();

        assertEquals(ModoLampada.OFF, lampada.getModo());
    }

    @Test
    public void testLampOn() {
        Lampada l = new Lampada();
        double consumoTotalAntes = l.getConsumoTotal();
        long stampAntes = l.getStamp();

        l.lampON();

        assertAll(
                () -> {
                    assertEquals(ModoLampada.ON, l.getModo());
                },
                () -> {
                    assertTrue(consumoTotalAntes < l.getConsumoTotal());
                },
                () -> {
                    assertTrue(stampAntes < l.getStamp());
                }
        );
    }
}
