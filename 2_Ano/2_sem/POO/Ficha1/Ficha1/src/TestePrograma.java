import java.util.Scanner;

public class TestePrograma {
    public static void main(String[] args) {
        Scanner sc = new Scanner(System.in);
        Ficha1 f1 = new Ficha1();

        // Pergunta 1
        /*
        System.out.print("Insira a temperatura em ºC: ");
        double temp = sc.nextDouble();
        double fahrenheit = f1.celsiusParaFarenheit(temp);
        System.out.println("Fahrenheit: " + fahrenheit);
        sc.close();
        */

        // Pergunta 2
        /*
        System.out.print("Insira um número: ");
        int a = sc.nextInt();
        System.out.print("Insira outro número: ");
        int b = sc.nextInt();
        int maior = f1.maior(a, b);
        System.out.println("Maior: " + maior);
        sc.close();
        */

        //Pergunta 3
        /*
        System.out.print("Insira o nome da conta: ");
        String nome = sc.next();
        System.out.print("Insira o saldo da conta: ");
        double saldo = sc.nextDouble();
        String descricao = f1.criaDescricaoConta(nome, saldo);
        System.out.println(descricao);
        sc.close();
        */

        //Pergunta 3
        /*
        System.out.print("Insira um valor em euros: ");
        double nome = sc.nextDouble();
        System.out.print("Insira a taxa de conversão Euros/Libras: ");
        double saldo = sc.nextDouble();
        double libras = f1.eurosParaLibras(nome, saldo);
        System.out.println("Libras: " + libras);
        sc.close();
        */

        //Pergunta 4
        /*
        System.out.print("Insira um número: ");
        int a = sc.nextInt();
        System.out.print("Insira outro número: ");
        int b = sc.nextInt();
        String text = f1.decrescMedia(a, b);
        System.out.println(text);
        sc.close();
        */

        //Pergunta 5
        /*
        System.out.print("Insira um número: ");
        int a = sc.nextInt();
        long fatorial = f1.factorial(a);
        System.out.println("Fatorial: " + fatorial);
        sc.close();
        */

        //Pergunta 6
        long tempo = f1.tempoGasto();
        System.out.println("Tempo gasto: " + tempo + "ms");
    }
}
