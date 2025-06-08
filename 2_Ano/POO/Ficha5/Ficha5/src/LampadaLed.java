public class LampadaLed extends Lampada {
    private int watts;
    private int lumens;

    public LampadaLed() {
        super();
        this.watts = 0;
        this.lumens = 0;
    }

    public LampadaLed(Modo x, double cpsE, double cpsO, double cT, double pC, int watts, int lumens) {
        super(x, cpsE, cpsO, cT, pC);
        this.watts = watts;
        this.lumens = lumens;
    }

    public LampadaLed(LampadaLed lampadaLed) {
        super(lampadaLed);
        this.watts = lampadaLed.watts;
        this.lumens = lampadaLed.lumens;
    }

    public int getWatts() {
        return watts;
    }

    public int getLumens() {
        return lumens;
    }

    public void setWatts(int watts) {
        this.watts = watts;
    }

    public void setLumens(int lumens) {
        this.lumens = lumens;
    }

    protected void atualizaConsumo() {
        long periodo = System.currentTimeMillis() - this.getStamp();

        if (this.getModo() == Modo.ON) {
            double consumoOn = (1.0 / this.watts) * this.getCpSOn();
            this.setConsumoTotal(this.getConsumoTotal() + consumoOn * periodo);
            this.setConsumoPeriodo(this.getConsumoPeriodo() + consumoOn * periodo);
        } else if (this.getModo() == Modo.ECO) {
            double consumoEco = (1.25 / this.watts) * this.getCpSEco();
            this.setConsumoTotal(this.getConsumoTotal() + consumoEco * periodo);
            this.setConsumoPeriodo(this.getConsumoPeriodo() + consumoEco * periodo);
        }

        this.setStamp(System.currentTimeMillis());
    }

    @Override
    public LampadaLed clone() {
        return new LampadaLed(this);
    }

    @Override
    public String toString() {
        StringBuilder sb = new StringBuilder();
        sb.append(super.toString());
        sb.append("\nWatts: ").append(this.watts)
                .append("\nLumens: ").append(this.lumens);
        return sb.toString();
    }

    @Override
    public boolean equals(Object o) {
        if (!super.equals(o)) return false;
        LampadaLed ll = (LampadaLed) o;
        return this.watts == ll.getWatts() &&
                this.lumens == ll.getLumens();
    }
}
