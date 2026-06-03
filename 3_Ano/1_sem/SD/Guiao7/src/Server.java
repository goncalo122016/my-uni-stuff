import java.io.DataInputStream;
import java.io.DataOutputStream;
import java.io.IOException;
import java.net.ServerSocket;
import java.net.Socket;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.HashMap;
import java.util.concurrent.locks.ReentrantLock;

import static java.util.Arrays.asList;

class ContactManager {
    private HashMap<String, Contact> contacts = new HashMap<>();
    private ReentrantLock lock = new ReentrantLock();

    // @TODO
    public void update(Contact c) {
        if (c == null) return;
        lock.lock();
        try {
            if (contacts.containsKey(c.name())) {
                contacts.replace(c.name(), c);
            } else {
                contacts.put(c.name(), c);
            }
        }
        finally {
            lock.unlock();
        }
    }

    // @TODO
    public ContactList getContacts() {
        lock.lock();
        try {
            ContactList list = new ContactList();
            list.addAll(contacts.values());
            return list;
        }
        finally {
            lock.unlock();
        }
    }
}

class ServerWorker implements Runnable {
    private Socket socket;
    private ContactManager manager;

    public ServerWorker(Socket socket, ContactManager manager) {
        this.socket = socket;
        this.manager = manager;
    }

    // @TODO
    @Override
    public void run() {
        try {
            DataInputStream in = new DataInputStream(socket.getInputStream());
            DataOutputStream out = new DataOutputStream(socket.getOutputStream());

            ContactList initialContacts = manager.getContacts();
            initialContacts.serialize(out);

            boolean running = true;
            while (running) {
                Contact contact = Contact.deserialize(in);
                if (contact == null) {
                    running = false;
                    break;
                }
                System.out.println(contact.toString());
                manager.update(contact);

                ContactList contacts = manager.getContacts();
                contacts.serialize(out);
            }

            System.out.println("Client disconnected.");

            socket.close();
        } catch (IOException e) {
            e.printStackTrace();
        }
    }
}



public class Server {

    public static void main (String[] args) throws IOException {
        ServerSocket serverSocket = new ServerSocket(12345);
        ContactManager manager = new ContactManager();
        // example pre-population
        manager.update(new Contact("John", 20, 253123321, null, asList("john@mail.com")));
        manager.update(new Contact("Alice", 30, 253987654, "CompanyInc.", asList("alice.personal@mail.com", "alice.business@mail.com")));
        manager.update(new Contact("Bob", 40, 253123456, "Comp.Ld", asList("bob@mail.com", "bob.work@mail.com")));

        while (true) {
            Socket socket = serverSocket.accept();
            Thread worker = new Thread(new ServerWorker(socket, manager));
            worker.start();
        }
    }

}
