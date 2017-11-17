package vortex;

import java.util.Arrays;
import java.util.Scanner;
import java.util.stream.Collectors;

public class Solution {
    int W;
    int H;
    int X;
    int[][] matrix;

    private void read() {
        Scanner in = new Scanner(System.in);
        W = in.nextInt();
        H = in.nextInt();
        X = in.nextInt();
        matrix = new int[H][];
        for (int i = 0; i < H; i++) {
            matrix[i] = new int[W];
            for (int j = 0; j < W; j++) {
                matrix[i][j] = in.nextInt();
            }
        }
    }

    public void rotate() {
        for (int i = 0; i < Math.min(W, H) / 2; i++) {
            rotateLayer(i);
        }
    }

    public static void main(String[] args) {
        Solution solution = new Solution();
        solution.read();
        solution.rotate();
        print(solution.matrix);
    }

    static class Pos {
        int row;
        int col;

        public Pos(int row, int col) {
            this.row = row;
            this.col = col;
        }
    }

    Pos[] positions(int layer) {
        int posCount = 2 * ( H - 2 * layer + W - 2 * layer - 2);
        Pos[] result = new Pos[posCount];
        int pInd = 0;
        for (int i = 0 + layer; i < H - layer; i++) {
            result[pInd++] = new Pos(i, layer);
        }
        for (int i = 0 + layer + 1; i < W - layer; i++) {
            result[pInd++] = new Pos(H - layer - 1, i);
        }
        for (int i = H - layer - 2; i >= 0 + layer; i--) {
            result[pInd++] = new Pos(i, W - layer - 1);
        }
        for (int i = W - layer - 2; i >= 0 + layer + 1; i--) {
            result[pInd++] = new Pos(layer, i);
        }
        return result;
    }

    void rotateLayer(int layer) {
        Pos[] pos = positions(layer);
        for (int i = 0; i < X; i++) {
            int val = get(pos[0]);
            for (int j = 1; j < pos.length; j++) {
                int tmp = get(pos[j]);
                set(pos[j], val);
                val = tmp;
            }
            set(pos[0], val);
        }
    }

    void set(Pos pos, int value) {
        matrix[pos.row][pos.col] = value;
    }

    int get(Pos pos) {
        return matrix[pos.row][pos.col];
    }

    static void print(int[][] matrix) {
        for (int i = 0; i < matrix.length; i++) {
            String s = Arrays.stream(matrix[i])
                    .mapToObj(Integer::toString)
                    .collect(Collectors.joining(" "));
            System.out.println(s);
        }
    }
}
