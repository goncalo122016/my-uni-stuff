public class Sensor {
    private double p;

    public Sensor(){
        this.p = 0;
    }

    public Sensor(double p) {
        this.p = p;
    }

    public Sensor(Sensor s){
        this.p = s.getP();
    }

    public double getP() {
        return p;
    }

    public boolean setP(double newP) {
        if (newP >= 0) {
            this.p = newP;
            return true;
        }
        return false;
    }

    public boolean equals(Sensor s){
        if (this == s){
            return true;
        }
        if ((s == null) || (this.getClass() != s.getClass())) {
            return false;
        }
        Sensor other = (Sensor) s;
        return ((this.p == other.getP()));
    }

    public String toString() {
        return "Pressão: " + this.p;
    }

    public Sensor clone() {
        return new Sensor(this);
    }
}
