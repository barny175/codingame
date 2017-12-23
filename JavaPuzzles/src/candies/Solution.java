package candies;

import java.util.ArrayList;
import java.util.List;
import java.util.Scanner;
import java.util.stream.Collectors;

public class Solution {

    public static void main(String args[]) {
        Scanner in = new Scanner(System.in);
        int n = in.nextInt();
        int k = in.nextInt();

        split(n, k).forEach(sr -> {
            System.out.println(subresToStr(sr));
        });
    }

    static String subresToStr(List<Integer> sr) {
        return sr.stream().map(Object::toString).collect(Collectors.joining(" "));
    }

    static List<List<Integer>> split(int n, int k) {

        List<List<Integer>> res = new ArrayList<>();
        if (n == 0) {
            List<Integer> sr = new ArrayList<>();
            res.add(sr);
            return res;
        }

        for (int i = 1; i <= k && i <= n; i++) {
            List<List<Integer>> subres = split(n - i, k);
            int j = i;
            subres.forEach(sr -> {
                System.err.println("Subres: " + subresToStr(sr));
                sr.add(0, j);

                res.add(sr);
            });
        }

        return res;
    }
}
