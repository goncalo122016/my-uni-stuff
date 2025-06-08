import java.time.LocalDateTime;
import java.time.temporal.ChronoUnit;

public class Ficha1 {
    public double celsiusParaFarenheit(double graus){
        return graus * 9/5 + 32;
    }

    public int maior (int a, int b){
        return a > b ? a : b;
    }

    public String criaDescricaoConta(String nome, double saldo){
        return("Nome: " + nome + "\nSaldo: " + saldo);
    }

    public double eurosParaLibras(double valor, double taxaConversao){
        return valor * taxaConversao;
    }

    public String decrescMedia (int a, int b){
        double media = (double)(a + b)/2;
        if (a>b) {
            return "Ordem: " + a + " -> " + b + "\nMédia: " + media;
        }
        else{
            return "Ordem: " + b + " -> " + a + "\nMédia: " + media;
        }
    }

    public long factorial(int num){
        if (num == 0) {
            return 1;
        }
        else{
            return num * factorial(num-1);
        }
    }

    public long tempoGasto(){
        LocalDateTime now = LocalDateTime.now();
        long res = factorial(5000);
        LocalDateTime then = LocalDateTime.now();

        return now.until(then, ChronoUnit.MICROS);
    }
}
