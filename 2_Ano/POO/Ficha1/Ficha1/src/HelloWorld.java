import java.util.Scanner;

public class HelloWorld {
    public static String greet(String name){
        return "Hello "+ name + "!";
    }

    public static void main(String[] args){
        Scanner sc = new Scanner(System.in);

        System.out.print("Enter a name: ");
        String name = sc.nextLine();
        String greeting=greet(name);

        System.out.println(greeting);
    }
}
