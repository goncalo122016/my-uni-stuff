package spotifum;

import spotifum.Menu.Menu;

/**
 * Main class of the Spotifum application.
 */
public class Main {

    /**
     * Main method that starts the application.
     *
     * @param args Command line arguments.
     */
    public static void main(String[] args) {
        Menu menu = new Menu();
        menu.start();
    }
}