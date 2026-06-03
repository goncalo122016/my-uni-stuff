package server;

import static org.junit.jupiter.api.Assertions.*;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.DisplayName;

import java.util.*;

@DisplayName("AuthService Tests Suite")
public class AuthServiceTestSuite {
    private AuthService authService;
    private Map<String, String> users;

    @BeforeEach
    public void setUp() {
        users = new HashMap<>();
        authService = new AuthService(users);
    }

    @Test
    @DisplayName("Test 1: Register new user")
    public void testRegisterNewUser() {
        assertTrue(authService.register("user1", "pass123"));
    }

    @Test
    @DisplayName("Test 2: Register duplicate user returns false")
    public void testRegisterDuplicate() {
        authService.register("user1", "pass123");
        assertFalse(authService.register("user1", "pass456"));
    }

    @Test
    @DisplayName("Test 3: Authenticate with correct password")
    public void testAuthenticateCorrect() {
        authService.register("user1", "pass123");
        assertTrue(authService.authenticate("user1", "pass123"));
    }

    @Test
    @DisplayName("Test 4: Authenticate with wrong password")
    public void testAuthenticateWrong() {
        authService.register("user1", "pass123");
        assertFalse(authService.authenticate("user1", "wrongpass"));
    }

    @Test
    @DisplayName("Test 5: Authenticate non-existent user")
    public void testAuthenticateNonExistent() {
        assertFalse(authService.authenticate("nonexistent", "pass"));
    }

    @Test
    @DisplayName("Test 6: Multiple users")
    public void testMultipleUsers() {
        authService.register("alice", "alice123");
        authService.register("bob", "bob456");
        authService.register("charlie", "charlie789");

        assertTrue(authService.authenticate("alice", "alice123"));
        assertTrue(authService.authenticate("bob", "bob456"));
        assertTrue(authService.authenticate("charlie", "charlie789"));
    }

    @Test
    @DisplayName("Test 7: Case sensitive usernames")
    public void testCaseSensitiveUsernames() {
        authService.register("User", "pass");
        assertFalse(authService.authenticate("user", "pass"));
    }

    @Test
    @DisplayName("Test 8: Case sensitive passwords")
    public void testCaseSensitivePasswords() {
        authService.register("user", "Pass");
        assertFalse(authService.authenticate("user", "pass"));
    }

    @Test
    @DisplayName("Test 9: Empty username")
    public void testEmptyUsername() {
        authService.register("", "pass");
        assertTrue(users.containsKey(""));
    }

    @Test
    @DisplayName("Test 10: Empty password")
    public void testEmptyPassword() {
        authService.register("user", "");
        assertTrue(authService.authenticate("user", ""));
    }

    @Test
    @DisplayName("Test 11: Long username")
    public void testLongUsername() {
        String longUser = "a".repeat(1000);
        assertTrue(authService.register(longUser, "pass"));
        assertTrue(authService.authenticate(longUser, "pass"));
    }

    @Test
    @DisplayName("Test 12: Long password")
    public void testLongPassword() {
        String longPass = "p".repeat(1000);
        assertTrue(authService.register("user", longPass));
        assertTrue(authService.authenticate("user", longPass));
    }

    @Test
    @DisplayName("Test 13: Special characters")
    public void testSpecialCharacters() {
        authService.register("user@123", "p@ss#word!");
        assertTrue(authService.authenticate("user@123", "p@ss#word!"));
    }

    @Test
    @DisplayName("Test 14: Unicode characters")
    public void testUnicodeCharacters() {
        authService.register("usuário", "senhaçã");
        assertTrue(authService.authenticate("usuário", "senhaçã"));
    }

    @Test
    @DisplayName("Test 15: Numbers only")
    public void testNumbersOnly() {
        authService.register("123456", "654321");
        assertTrue(authService.authenticate("123456", "654321"));
    }

    @Test
    @DisplayName("Test 16: Sequential registration and authentication")
    public void testSequentialOps() {
        for (int i = 0; i < 10; i++) {
            String user = "user" + i;
            String pass = "pass" + i;
            assertTrue(authService.register(user, pass));
            assertTrue(authService.authenticate(user, pass));
        }
    }

    @Test
    @DisplayName("Test 17: Whitespace in credentials")
    public void testWhitespace() {
        authService.register("user name", "pass word");
        assertTrue(authService.authenticate("user name", "pass word"));
    }

    @Test
    @DisplayName("Test 18: Verify all users stored")
    public void testAllUsersStored() {
        authService.register("alice", "a1");
        authService.register("bob", "b2");
        assertEquals(2, users.size());
    }

    @Test
    @DisplayName("Test 19: Register and verify password stored")
    public void testPasswordStored() {
        authService.register("user", "mypass");
        assertEquals("mypass", users.get("user"));
    }

    @Test
    @DisplayName("Test 20: Concurrent registrations")
    public void testConcurrentRegistrations() throws InterruptedException {
        Map<String, String> syncUsers = Collections.synchronizedMap(new HashMap<>());
        AuthService concurrentAuth = new AuthService(syncUsers);

        Thread[] threads = new Thread[10];
        for (int i = 0; i < 10; i++) {
            final int index = i;
            threads[i] = new Thread(() -> {
                concurrentAuth.register("user" + index, "pass" + index);
            });
            threads[i].start();
        }

        for (Thread t : threads) {
            t.join();
        }

        assertTrue(syncUsers.size() >= 10);
    }
}
