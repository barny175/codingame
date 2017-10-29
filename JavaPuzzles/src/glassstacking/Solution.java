package glassstacking;

import java.util.Scanner;

public class Solution {
    public static final String[] GLASS = {
            " *** ",
            " * * ",
            " * * ",
            "*****"};

    private static int levels(int n) {
        int res = (int) Math.floor(Math.sqrt(1/4 + 2 * n) - 1/2);
        return (res * (res + 1) / 2) > n ? res - 1 : res;
    }

    private static String glass(int n, int levels) {
        StringBuilder sb = new StringBuilder();
        for (int i = 0; i < GLASS.length; i++) {
            for (int j = 0; j < levels - n; j++) {
                sb.append("   ");
            }
            sb.append(GLASS[i]);
            for (int j = 0; j < n - 1; j++) {
                sb.append(" ").append(GLASS[i]);
            }
            for (int j = 0; j < levels - n; j++) {
                sb.append("   ");
            }
            sb.append("\n");

        }
        return sb.toString();
    }

    private static String draw(int levels) {
        StringBuilder sb = new StringBuilder();
        for (int i = 1; i <= levels; i++) {
            sb.append(glass(i, levels));
        }
        return sb.toString();
    }

    public static void main(String args[]) {
        Scanner in = new Scanner(System.in);
        int N = in.nextInt();
//        int N = 10;
        String res = draw(levels(N));
        System.out.println(res.substring(0, res.length() - 1));

    }
}
