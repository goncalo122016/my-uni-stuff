import java.util.Scanner;
import java.time.LocalDateTime;

public class Main {
    public static void main(String[] args) {
        Scanner sc = new Scanner(System.in);
        System.out.println("Insere o número do Exercício :");
        int n = sc.nextInt();
        switch (n) {
            case 1:
                Circulo c1 = new Circulo();
                Circulo c2 = c1.clone();

                /*
                System.out.println(c1);
                c1.alteraCentro(1,2);
                c1.setR(3);
                System.out.println(c1);
                double area = c1.calculaArea();
                System.out.println("Area: " + area);
                double perimetro = c1.calculaPerimetro();
                System.out.println("Perimetro: " + perimetro);
                */

                System.out.println(c1 == c2);
                System.out.println(c1.equals(c2));

                break;
            case 2:
                Sensor s1 = new Sensor();

                System.out.println(s1);
                s1.setP(sc.nextDouble());
                System.out.println(s1.getP());
                break;
            case 3:
                String[] lyrics = {
                        "This is the first line",
                        "This is the second line",
                        "This is the third line"
                };

                String[] musicalNotes = {
                        "C", "D", "E", "F", "G"
                };

                Musica m1 = new Musica("Song Name", "Artist Name", "Author Name", "Editor Name", lyrics, musicalNotes, 180, 3);

                System.out.println(m1);
                m1.addLetra(2, "Isto deixou de ser a Terceira Linha!");
                System.out.println(m1);

                System.out.println("Linha mais longa: " + m1.linhaMaisLonga());
            case 9:
                LinhaEncomenda linha1 = new LinhaEncomenda("REF001", "Produto 1", 100.0, 2, 0.23, 0.10);
                LinhaEncomenda linha2 = new LinhaEncomenda("REF002", "Produto 2", 50.0, 5, 0.23, 0.05);
                LinhaEncomenda linha3 = new LinhaEncomenda("REF003", "Produto 3", 20.0, 10, 0.23, 0.0);

                System.out.println("Linha 1: \n" + linha1.toString());
                System.out.println("Valor total linha 1: " + linha1.calculaValorLinhaEnc());
                System.out.println("Valor de desconto linha 1: " + linha1.calculaValorDesconto());

                System.out.println("\nLinha 2: \n" + linha2.toString());
                System.out.println("Valor total linha 2: " + linha2.calculaValorLinhaEnc());
                System.out.println("Valor de desconto linha 2: " + linha2.calculaValorDesconto());

                System.out.println("\nLinha 3: \n" + linha3.toString());
                System.out.println("Valor total linha 3: " + linha3.calculaValorLinhaEnc());
                System.out.println("Valor de desconto linha 3: " + linha3.calculaValorDesconto());

                LinhaEncomenda[] linhas = {linha1, linha2, linha3};

                Encomenda encomenda = new Encomenda("Cliente X", 123456789, "Rua A, 100", 1, LocalDateTime.now(), linhas);

                System.out.println("\nEncomenda: \n" + encomenda.toString());
                System.out.println("Valor total da encomenda: " + encomenda.calculaValorTotal());
                System.out.println("Valor total de desconto da encomenda: " + encomenda.calculaValorDesconto());
                System.out.println("Número total de produtos: " + encomenda.numeroTotalProdutos());

                System.out.println("\nExiste o produto REF001 na encomenda? " + encomenda.existeProdutoEncomenda("REF001"));
                System.out.println("Existe o produto REF999 na encomenda? " + encomenda.existeProdutoEncomenda("REF999"));
                break;
        }
    }
}