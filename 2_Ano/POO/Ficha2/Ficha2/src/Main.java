import java.time.LocalDate;
import java.util.Arrays;
import java.util.Scanner;

public class Main {
    public static int[] readArray (){
        System.out.println("Enter array size: ");
        Scanner sc = new Scanner(System.in);
        int size = sc.nextInt();
        int[] res = new int[size];
        for (int i = 0; i < res.length; i++) {
            res[i] = sc.nextInt();
        }
        sc.nextLine();

        return res;
    }

    public static String[] readStringArray(){
        System.out.println("Enter array size: ");
        Scanner sc = new Scanner(System.in);
        int size = sc.nextInt();
        sc.nextLine();
        String[] res = new String[size];
        for (int i = 0; i < res.length; i++) {
            res[i] = sc.nextLine();
        }
        return res;
    }

    public static int[][] readMatrix (){
        Scanner sc = new Scanner(System.in);
        System.out.println("Enter matrix number of lines: ");
        int l = sc.nextInt();
        System.out.println("Enter matrix number of columns: ");
        int c = sc.nextInt();
        int[][] res = new int[l][c];
        for (int i = 0; i < l; i++) {
            for (int j = 0; j < c; j++) {
                res[i][j] = sc.nextInt();
            }
        }
        System.out.println("Enter matrix elements: ");
        for (int i = 0; i < l; i++) {
            for (int j = 0; j < c; j++) {
                System.out.print(res[i][j] + " ");
            }
            System.out.println();
        }
        return res;
    }

    public static void main(String[] args) {
        Ficha2 f2 = new Ficha2();
        Dates d = new Dates();
        Scanner sc = new Scanner(System.in);
        System.out.println("Insira o número do Exercício: ");
        int ex = sc.nextInt();

        switch (ex) {
            case 11:
                int[] array = readArray();

                int min = f2.min(array);
                System.out.println("O número mínimo é: " + min);
                break;
            case 12:
                System.out.println("Introduz o primeiro valor: ");
                int a = sc.nextInt();
                System.out.println("Introduz o segundo valor: ");
                int b = sc.nextInt();

                int[] r = f2.buildBetween(a,b);
                System.out.println(Arrays.toString(r));
                break;
            case 13:
                int[] a1 = readArray();
                int[] a2 = readArray();

                int[] result = f2.equals(a1,a2);

                System.out.println(Arrays.toString(result));
                break;
            case 2:
                System.out.println("Introduz o tamanho: ");
                int size = sc.nextInt();

                d.initDates(size);

                d.printDates();

                d.insertDate(LocalDate.now());

                d.printDates();

                break;
            case 3:
                int[] a3 = readArray();
                System.out.println("Array original: " + Arrays.toString(a3));

                f2.sort(a3);
                System.out.println("Array ordenado: " + Arrays.toString(a3));

                int ans = f2.binarySearch(a3, 3);
                System.out.println("Resultado da procura: " + ans);
                break;
            case 4:
                String[] a4 = readStringArray();
                System.out.println("Array original: " + Arrays.toString(a4));

                String[] answer = f2.uniqueStrings(a4);
                System.out.println("Retirar repetidos: " + Arrays.toString(answer));
                String longest = f2.longestString(a4);
                System.out.println("Maior string: " + longest);
                String[] morethanonce = f2.moreThanOnce(a4);
                System.out.println("Morethan once: " + Arrays.toString(morethanonce));
                System.out.println("'Pila' ocorre: " + f2.nOcorre(a4, "Pila") + " vezes!");
                break;
            case 5:
                int[][] m1 = readMatrix();
                int[][] m2 = readMatrix();

                int[][] m3 = f2.addMatrix(m1,m2);
                if (m3 == null) {
                    System.out.println("As matrizes têm de ter as mesmas dimensões!");
                    break;
                }
                System.out.println("Matriz soma: ");
                for (int i = 0; i < m3.length; i++) {
                    for (int j = 0; j < m3[i].length; j++) {
                        System.out.print(m3[i][j] + " ");
                    }
                    System.out.println();
                }
                break;
            case 7:
                int[] chaves = f2.geraAleatorioEntre(5, 1, 50);
                int[] estrelas = f2.geraAleatorioEntre(2, 1, 9);

                int[] c = new int[5], e = new int[2];
                System.out.println("Escolha 5 chaves entre 1 e 50: ");
                for (int i = 0; i < 5; i++) {
                    c[i] = sc.nextInt();
                }
                System.out.println("Escolha 2 estrelas entre 1 e 9: ");
                for (int i = 0; i < 2; i++) {
                    e[i] = sc.nextInt();
                }
                if (Arrays.equals(chaves, c) && Arrays.equals(estrelas, e)) {
                    System.out.println("PARABÉNS GANHOU !!!");
                }
                else{
                    System.out.println("MAIS SORTE PARA A PŔOXIMA SEU MERDAS!");
                }
                System.out.println("Chaves: " + Arrays.toString(chaves));
                System.out.println("Estrelas: " + Arrays.toString(estrelas));
                break;
        }
        sc.close();
    }
}