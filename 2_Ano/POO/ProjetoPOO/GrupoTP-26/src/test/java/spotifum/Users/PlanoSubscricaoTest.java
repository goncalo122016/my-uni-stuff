package spotifum.Users;

import static org.junit.jupiter.api.Assertions.assertEquals;
import org.junit.jupiter.api.Test;

/**
 * The tests for the PlanoSubscricao class.
 *
 * This class contains tests for the points calculation logic of the PlanoSubscricao enum values,
 * including FREE, PremiumBase, and PremiumTop plans.
 *
 * @author  Afonso Martins (a106931), Luís Felício (a106913) and Goncalo Castro (a107337)
 * @version (17052025)
 */
public class PlanoSubscricaoTest {

    /**
     * Tests the FREE plan's point calculation.
     */
    @Test
    public void testFreePlanCalculaPontos() {
        double basePoints = 0;
        double expected = 5; // 5 points per song
        double result = PlanoSubscricao.FREE.calcularPontos(basePoints);
        assertEquals(expected, result, 0.001, "FREE should return 5 points per song.");
    }

    /**
     * Tests the PremiumBase plan's point calculation.
     */
    @Test
    public void testPremiumBaseCalculaPontos() {
        double basePoints = 0;
        double expected = 10; // 10 points per song
        double result = PlanoSubscricao.PremiumBase.calcularPontos(basePoints);
        assertEquals(expected, result, 0.001, "PremiumBase should return 10 points per song.");
    }

    /**
     * Tests the PremiumTop plan's point calculation with existing accumulated points.
     */
    @Test
    public void testPremiumTopCalculaPontos() {
        double basePoints = 100;
        double expected = 102.5; // 100 + 2.5% of 100
        double result = PlanoSubscricao.PremiumTop.calcularPontos(basePoints);
        assertEquals(expected, result, 0.001, "PremiumTop should return base points plus 2.5%.");
    }

    /**
     * Tests the plans when the initial points are zero.
     */
    @Test
    public void testValoresZero() {
        Utilizador u1 = new Utilizador("u1", "email", "morada", PlanoSubscricao.FREE);
        assertEquals(0, u1.getPontos(), 0.001, "FREE with 0 should return 0.");
        u1 = new Utilizador("u1", "email", "morada", PlanoSubscricao.PremiumBase);
        assertEquals(0, u1.getPontos(), 0.001, "PremiumBase with 0 should return 0.");
        u1 = new Utilizador("u1", "email", "morada", PlanoSubscricao.PremiumTop);
        assertEquals(100, u1.getPontos(), 0.001, "PremiumTop with 0 should return 100.");
    }

    /**
     * Tests the plans with negative point values.
     */
    @Test
    public void testValoresNegativos() {
        double negativePoints = -100;
        assertEquals(0, PlanoSubscricao.FREE.calcularPontos(negativePoints), 0.001, "FREE with negative should return 0.");
        assertEquals(0, PlanoSubscricao.PremiumBase.calcularPontos(negativePoints), 0.001, "PremiumBase with negative should return 0.");
        assertEquals(0, PlanoSubscricao.PremiumTop.calcularPontos(negativePoints), 0.001, "PremiumTop with negative should return 0.");
    }
}