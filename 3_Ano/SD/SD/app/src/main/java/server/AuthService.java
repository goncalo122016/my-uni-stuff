package server;

import java.util.Map;

public class AuthService {
    Map<String , String> users;

    public AuthService(Map<String, String> users) {
        this.users = users;
    }

    public boolean authenticate(String username, String password) {
        return users.containsKey(username) && users.get(username).equals(password);
    }

    public boolean register(String username, String password) {
        if (users.containsKey(username)) {
            return false; // User already exists
        }
        users.put(username, password);
        return true;
    }
}
