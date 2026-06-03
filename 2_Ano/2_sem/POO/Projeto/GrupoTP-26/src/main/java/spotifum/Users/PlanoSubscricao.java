package spotifum.Users;

/**
 * Enum representing different subscription plans in the system.
 * Each plan has a specific method to calculate points based on a base value.
 */

public enum PlanoSubscricao {
    /**
     * Free subscription plan.
     * Calculates points by returning the base value (multiplier of 1).
     */
    FREE {
        @Override
        public double calcularPontos(double pontos) {
            if (pontos < 0) { return 0;}
            return pontos + 5;
        }
    },

    /**
     * Base premium subscription plan.
     * Calculates points by returning twice the base value (multiplier of 2).
     */
    PremiumBase {
        @Override
        public double calcularPontos(double pontos) {
            if (pontos < 0) { return 0;}
            return pontos + 10;
        }
    },

    /**
     * Top premium subscription plan.
     * Calculates points by returning three times the base value (multiplier of 3).
     */
    PremiumTop {
        @Override
        public double calcularPontos(double pontos) {
            if (pontos < 0) { return 0;}
            return pontos + pontos * 0.025;
        }
    };

    /**
     * Abstract method to calculate points based on a base value.
     * Each subscription plan implements this method according to its own rules.
     *
     * @param pontos The base value for points calculation.
     * @return The calculated points based on the subscription plan type.
     */
    public abstract double calcularPontos(double pontos);
}