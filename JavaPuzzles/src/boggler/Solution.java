package boggler;

import java.util.ArrayList;
import java.util.List;
import java.util.Scanner;
import java.util.Set;
import java.util.stream.IntStream;

import static java.util.stream.Collectors.toList;
import static java.util.stream.Collectors.toSet;

public class Solution {
    private char[][] matrix;

    public Solution(char[][] matrix) {
        this.matrix = matrix;
    }

    public static void main(String args[]) {
        Scanner in = new Scanner(System.in);

        char[][] matrix = new char[4][];
        for (int i = 0; i < 4; i++) {
            matrix[i] = in.next().toCharArray();
        }

        Solution solution = new Solution(matrix);
        int n = in.nextInt();
        for (int i = 0; i < n; i++) {
            String w = in.next();
            System.out.println(solution.matches(w) ? "true" : "false");
        }
    }

    public boolean matches(String word) {
        List<Match> matches = initialMatches(word);

        for (int i = 1; i < word.length(); i++) {
            matches = nextMatches(word.charAt(i), matches);
            if (matches.isEmpty()) {
                return false;
            }
        }

        return !matches.isEmpty();
    }

    List<Match> nextMatches(char c, List<Match> matches) {
        return matches.stream()
                    .flatMap(match -> {
                        Set<Pos> neighbours = neighbours(match.last().row, match.last().col);
                        neighbours.removeAll(match.used);
                        return neighbours.stream()
                                .filter(pos -> matrix[pos.row][pos.col] == c)
                                .map(pos -> {
                                    Match newMatch = new Match();
                                    newMatch.used.addAll(match.used);
                                    newMatch.used.add(pos);
                                    return newMatch;
                                });
                    })
                    .collect(toList());
    }

    static class Match {
        List<Pos> used = new ArrayList<>();
        Pos last() {
            return used.get(used.size() - 1);
        }
    }

    static class Pos {
        int row;
        int col;

        public Pos(int row, int col) {
            this.row = row;
            this.col = col;
        }

        @Override
        public boolean equals(Object o) {
            if (this == o) return true;
            if (o == null || getClass() != o.getClass()) return false;

            Pos pos = (Pos) o;

            if (row != pos.row) return false;
            return col == pos.col;
        }

        @Override
        public int hashCode() {
            int result = row;
            result = 31 * result + col;
            return result;
        }
    }

    Set<Pos> neighbours(int row, int col) {
        return IntStream.rangeClosed(row - 1, row + 1)
                .mapToObj(r -> {
                    return IntStream.rangeClosed(col - 1, col + 1)
                            .mapToObj(c -> new Pos(r, c));
                })
                .flatMap(s -> s)
                .filter(p -> p.row >= 0 && p.row < matrix.length)
                .filter(p -> p.col >= 0 && p.col < matrix.length)
                .filter(p -> p.row != row || p.col != col)
                .collect(toSet());
    }

    List<Match> initialMatches(String word) {
        if (word.isEmpty()) {
            return new ArrayList<>();
        }
        return IntStream.range(0, matrix.length)
                .mapToObj(r -> {
                    return IntStream.range(0, matrix.length)
                            .mapToObj(c -> new Pos(r, c));
                })
                .flatMap(s -> s)
                .filter(pos -> matrix[pos.row][pos.col] == word.charAt(0))
                .map(ps -> {
                    Match match = new Match();
                    match.used.add(ps);
                    return match;
                })
                .collect(toList());
    }
}
