import java.util.Arrays;
import  java.lang.Math;
import java.util.Random;

public class Ficha2 {
    public int min(int[] a) {
        int min = a[0];
        for (int i = 1; i < a.length; i++) {
            if (a[i] < min) {
                min = a[i];
            }
        }
        return min;
    }

    public int[] buildBetween(int a, int b) {
        int dif = Math.abs(a - b) + 1;
        int[] res = new int[dif];
        if (a < b) {
            for (int i = 0; i < res.length; i++) {
                res [i] = a + i;
            }
        }
        return res;
    }

    public int[] equals(int[] a, int[] b) {
        int[] res = new int[Math.min(a.length, b.length)];
        int count = 0;

        for (int i = 0; i < a.length; i++) {
            for (int j = 0; j < b.length; j++) {
                boolean exists = false;
                for (int k = 0; k < count && !exists; k++) {
                    if (a[i] == res[k]) {
                        exists = true;
                    }
                }
                if (!exists) {
                    res[count] = a[i];
                    count++;
                }
            }
        }
        return Arrays.copyOf(res, count);
    }

    public void sort(int[] a) {
        Arrays.sort(a);
    }

    public int binarySearch(int[] a, int target) {
        int low = 0;
        int high = a.length - 1;
        while (low <= high) {
            int mid = (low + high) / 2;
            if (a[mid] == target) {
                return mid;
            }
            if (a[mid] < target) {
                low = mid + 1;
            }
            else {
                high = mid - 1;
            }
        }
        return -1;
    }

    private boolean exists(String s, String[] words, int size) {
        for (int i = 0; i < size; i++) {
            if (s.equals(words[i])) {
                return true;
            }
        }
        return false;
    }

    public String[] uniqueStrings(String[] a) {
        String[] res = new String[a.length];
        int count = 0;
        for (int i = 0; i < a.length; i++) {
            if (!exists(a[i], res, count)){
                res[count++] = a[i];
            }
        }
        return Arrays.copyOf(res, count);
    }

    public String longestString(String[] a) {
        String max = a[0];
        for (int i = 1; i < a.length; i++) {
            if (a[i].length() > max.length()) {
                max = a[i];
            }
        }
        return max;
    }

    public String[] moreThanOnce(String[] a) {
        int size = 0, count = 0;
        String[] aux = new String[a.length];
        String[] res = new String[a.length];
        for (int i = 0; i < a.length; i++) {
            if (exists(a[i], aux, count)) {
                res[size++] = a[i];
            }
            else {
                aux[count++] = a[i];
            }
        }
        return Arrays.copyOf(res, size);
    }

    public int nOcorre (String[] a, String b) {
        int count = 0;
        for (int i = 0; i < a.length; i++) {
            if (a[i].equals(b)) {
                count++;
            }
        }
        return count;
    }

    public int[][] addMatrix(int[][] a, int[][] b) {
        if (a.length != b.length || a[0].length != b[0].length) {
            return null;
        }

        int[][] res = new int[a.length][a[0].length];
        for (int i = 0; i < a.length; i++) {
            for (int j = 0; j < a[0].length; j++) {
                res[i][j] = a[i][j] + b[i][j];
            }
        }
        return res;
    }

    public int[] geraAleatorioEntre(int n, int i, int j) {
        int[] chaves = new int[n];
        for (int k = 0; k<n; k++){
            int rand = (int) (i + Math.random() * Math.abs(j-i));
            chaves[k] = rand;
        }
        return chaves;
    }
}
