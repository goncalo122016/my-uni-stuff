public class Circulo {
    private double x;
    private double y;
    private double r;

    public void alteraCentro(double x, double y){
        this.x = x;
        this.y = y;
    }

    public double calculaArea() {
        return Math.PI * Math.pow(this.r, 2);
    }

    public double calculaPerimetro () {
        return 2 * Math.PI * this.r;
    }

    // Constructors
    public Circulo() {
        this.x = 0;
        this.y = 0;
        this.r = 0;
    }

    public Circulo(double x, double y, double r) {
        this.x = x;
        this.y = y;
        this.r = r;
    }

    public Circulo(Circulo c) {
        this.x = c.getX();
        this.y = c.getY();
        this.r = c.getR();
    }

    // Getters and Setters
    public double getX() {
        return this.x;
    }

    public void setX(double newX) {
        this.x = newX;
    }

    public double getY() {
        return this.y;
    }

    public void setY(double newY) {
        this.y = newY;
    }

    public double getR() {
        return this.r;
    }

    public void setR(double newR) {
        this.r = newR;
    }

    public boolean equals(Circulo c) {
        if (this == c){
            return true;
        }
        if ((c == null) || (this.getClass() != c.getClass())) {
            return false;
        }
        Circulo other = (Circulo) c;
        return  ((this.x == other.getX()) && (this.y == other.getY()) && (this.r == other.getR()));
    }

    public String toString() {
        return "Centro: (" + this.x + ", " + this.y + ") e Raio: " + this.r;
    }

    public Circulo clone() {
        return new Circulo(this);
    }
}
