package common;

public class Utils {
    public boolean readLineYesOrNo(String input) {
        if (input == null) return false;
        String s = input.trim().toLowerCase();
        return s.equals("s") || s.equals("y") || s.equals("sim") || s.equals("yes");
    }
}