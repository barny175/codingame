package textalignment;
import java.util.*;
import java.io.*;
import java.math.*;
import java.util.stream.*;

class Solution {

    public static void main(String args[]) {
//        Scanner in = new Scanner(System.in);
        Scanner in = new Scanner("Never gonna give you up, never gonna let you down\n" +
                "Never gonna run around and desert you\n" +
                "Never gonna make you cry, never gonna say goodbye\n" +
                "Never gonna tell a lie and hurt you");
//        String alignment = in.nextLine();
//        int N = in.nextInt();
//        if (in.hasNextLine()) {
//            in.nextLine();
//        }
        String alignment = "JUSTIFY";
        int N = 4;


        ArrayList<String> text = new ArrayList<>();
        for (int i = 0; i < N; i++) {
            text.add(in.nextLine());
        }

        if (alignment.equals("LEFT")) {
            text.stream()
                    .forEach(s -> System.out.println(s));
        } else if (alignment.equals("RIGHT")) {
            int len = longest(text);
            text.stream()
                    .forEach(s -> {
                        System.out.println(spaces(len - s.length()) + s);
                    });
        } else if (alignment.equals("CENTER")){
            int len = longest(text);
            text.stream()
                    .forEach(s -> {
                        System.out.println(spaces((len - s.length()) / 2) + s);
                    });
        } else {
            int len = longest(text);
            text.stream()
                    .map(s -> {
                        return s.split(" ");
                    })
                    .forEach(s -> {
                        System.out.println(split(len, s));
                    });
        }

    }

    private static String split(int len, String[] s) {
        int gaps = s.length - 1;
        int spacesToSpare = len - len(s);
        double gap = spacesToSpare / (double) gaps;
        StringBuilder sb = new StringBuilder();
        int usedSpaces = 0;
        for (int i = 0; i < gaps; i++) {
            sb.append(s[i]);
            int spaceCount = (int) Math.floor(gap * (i+1));
            sb.append(spaces(spaceCount - usedSpaces));
            usedSpaces = spaceCount;
        }
        sb.append(s[gaps]);
        return sb.toString();
    }

    private static int len(String[] sa) {
        int len = 0;
        for(String s : sa)
            len += s.length();
        return len;
    }
    private static String spaces(int n) {
        return IntStream.range(0, n).mapToObj(i -> " ")
                .collect(Collectors.joining());
    }

    private static int longest(List<String> text) {
        return text.stream().mapToInt(s -> s.length())
                .max()
                .getAsInt();
    }
}
