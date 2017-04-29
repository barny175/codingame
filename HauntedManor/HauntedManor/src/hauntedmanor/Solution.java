/*
 * To change this license header, choose License Headers in Project Properties.
 * To change this template file, choose Tools | Templates
 * and open the template in the editor.
 */
package hauntedmanor;

import java.io.StringReader;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Scanner;
import java.util.stream.Collectors;
import java.util.stream.IntStream;

/**
 *
 * @author mbarnas
 */
public class Solution {
	private final int size;
	Map<Direction, List<Integer>> canSee;
	char[][] manor;
	private List<Equation> equations;
	Map<Creature, Integer> counts;

	public Solution(int size, Map<Direction, List<Integer>> canSee, char[][] manorTemplate, Map<Creature, Integer> counts) {
		equations = new ArrayList<>(4 * size);
		this.size = size;
		this.canSee = canSee;
		this.manor = manorTemplate;
		compileEquations();
		this.counts = counts;
	}

	public static void main(String[] args) {
		StringReader sr = new StringReader("5 14 5\n" +
"7\n" +
"2 6 4 7 2 4 2\n" +
"0 0 0 4 0 0 0\n" +
"2 4 0 1 1 1 0\n" +
"4 4 2 4 4 1 5\n" +
"..\\....\n" +
"..../..\n" +
"/.//\\.\\\n" +
"/\\./\\\\.\n" +
"\\\\.\\\\.\\\n" +
"\\\\/.../\n" +
"./\\./\\.");
//		Scanner in = new Scanner(System.in);
		Scanner in = new Scanner(sr);
		Map<Creature, Integer> counts = new HashMap<>();
		counts.put(Creature.Vampire, in.nextInt());
		counts.put(Creature.Zombie, in.nextInt());
		counts.put(Creature.Ghost, in.nextInt());
		int size = in.nextInt();
		Map<Direction, List<Integer>> canSee = new HashMap<>();
		canSee.put(Direction.Down, readInts(size, in));
		canSee.put(Direction.Up, readInts(size, in));
		canSee.put(Direction.Right, readInts(size, in));
		canSee.put(Direction.Left, readInts(size, in));
		in.nextLine();
		char[][] manorTemplate = new char[size][];
		for (int i = 0; i < size; i++) {
			manorTemplate[i] = in.nextLine().toCharArray();
		}

		Solution manor = new Solution(size, canSee, manorTemplate, counts);
		manor.find();
	}

	private static List<Integer> readInts(int size, Scanner in) {
		List<Integer> list = new ArrayList<>();
		for (int i = 0; i < size; i++) {
			list.add(in.nextInt());
		}
		return list;
	}

	Position initialPos(int rowOrCol, Direction direction) {
		switch (direction) {
			case Up:
				return new Position(size - 1, rowOrCol, direction);
			case Down:
				return new Position(0, rowOrCol, direction);
			case Left:
				return new Position(rowOrCol, size - 1, direction);
			case Right:
			default:
				return new Position(rowOrCol, 0, direction);
		}
	}

	boolean isValid(Position pos) {
		return pos.row >= 0 && pos.row < size && pos.col >= 0 && pos.col < size;
	}

	Position move(Position pos) {
		Direction direction = pos.direction;
		if (isMirror(pos)) {
			direction = direction.mirror(manor[pos.row][pos.col]);
		}

		Position newPos = new Position(pos.row, pos.col, direction);
		switch (direction) {
			case Up:
				newPos.row = pos.row - 1;
				break;
			case Down:
				newPos.row = pos.row + 1;
				break;
			case Left:
				newPos.col = pos.col - 1;
				break;
			case Right:
				newPos.col = pos.col + 1;
				break;
		}

		return newPos;
	}

	boolean isMirror(Position pos) {
		char c = manor[pos.row][pos.col];
		return (c == '/' || c == '\\');
	}

	boolean isVisible(Position pos, boolean mirror) {
		char creature = manor[pos.row][pos.col];
		switch (creature) {
			case 'V':
				return !mirror;
			case 'G':
				return mirror;
			case 'Z':
				return true;
			default:
				return false;
		}
	}

	Equation compileEquation(int rowOrCol, Direction direction, int result) {
		boolean mirror = false;
		Position pos = initialPos(rowOrCol, direction);
		Equation equation = new Equation(rowOrCol, direction);
		while (isValid(pos)) {
			if (isMirror(pos)) {
				mirror = true;
			} else {
				equation.addElement(new EquationElement(pos.row, pos.col, mirror));
			}

			pos = move(pos);
		}
		equation.result = result;
		return equation;
	}

	private void compileEquations() {
		this.equations = this.canSee.entrySet().stream()
				.flatMap(e -> {
					Direction dir = e.getKey();
					List<Integer> counts = e.getValue();
					return IntStream.range(0, counts.size())
					.mapToObj(i -> compileEquation(i, dir, counts.get(i)));
				})
				.filter(e -> e.elements.size() > 0)
				.sorted((e1, e2) -> Integer.compare(e1.elements.size(), e2.elements.size()))
				.collect(Collectors.toList());
	}

	private void print() {
		for (char[] line : manor) {
			for (int j = 0; j < line.length; j++) {
				System.out.print(line[j]);
			}
			System.out.println();
		}
	}

	private void find() {
		findSolution(0, 0, 0);
	}

	private void completeWithUnusedCreatures() {
		this.counts.entrySet().stream()
				.filter(e -> e.getValue() > 0)
				.findFirst()
				.ifPresent(e -> {
					for (char[] line : manor) {
						for (int i = 0; i < line.length; i++) {
							if (line[i] == '.')
								line[i] = e.getKey().getChar();
						}
					}
				});
	}

	private class Equation {

		Direction dir;
		int rowCol;

		public Equation(int rowCol, Direction dir) {
			this.dir = dir;
			this.rowCol = rowCol;
		}

		int result;
		List<EquationElement> elements = new ArrayList<>();

		void addElement(EquationElement element) {
			elements.add(element);
		}

		@Override
		public String toString() {
			return "Equation{" + "dir=" + dir + ", rowCol=" + rowCol + ", result=" + result + '}';
		}
	}

	private class EquationElement {

		int row;
		int col;
		boolean mirror;

		public EquationElement(int row, int col, boolean mirror) {
			this.row = row;
			this.col = col;
			this.mirror = mirror;
		}

		boolean isVisible(Creature c) {
			if (mirror) {
				return c.visibleInMirror;
			} else {
				return c.directlyVisible;
			}
		}

		@Override
		public String toString() {
			return "EquationElement{" + "row=" + row + ", col=" + col + ", mirror=" + mirror + '}';
		}
	}

	boolean findSolution(int equationIndex, int elementIndex, int currentSum) {
		if (equationIndex == this.equations.size()) {
			completeWithUnusedCreatures();
			print();
			return true;
		}
		
		Equation equation = this.equations.get(equationIndex);
		if (elementIndex == equation.elements.size()) {
			if (currentSum == equation.result) {
				return findSolution(equationIndex + 1, 0, 0);
			} else {
				return false;
			}
		}
		
		EquationElement el = equation.elements.get(elementIndex);
		if (manor[el.row][el.col] == '.') {
			for (Creature c : Creature.values()) {
				if (counts.get(c) > 0) {
					if (el.isVisible(c) && (currentSum+1) > equation.result)
						continue;

					counts.put(c, counts.get(c) - 1);
					manor[el.row][el.col] = c.getChar();
					if (findSolution(equationIndex, elementIndex + 1, currentSum + (el.isVisible(c) ? 1 : 0))) {
						return true;
					}
					counts.put(c, counts.get(c) + 1);
					manor[el.row][el.col] = '.';
				}
			}
			return false;
		} 
		currentSum += el.isVisible(Creature.fromChar(manor[el.row][el.col])) ? 1 : 0;
		return findSolution(equationIndex, elementIndex + 1, currentSum);
	}

	enum Creature {

		Zombie(true, true),
		Vampire(true, false),
		Ghost(false, true);

		static Creature fromChar(char first) {
			for (Creature c : Creature.values()) {
				if (c.name().charAt(0) == first) {
					return c;
				}
			}
			throw new IllegalArgumentException();
		}

		boolean directlyVisible;
		boolean visibleInMirror;

		Creature(boolean directlyVisible, boolean visibleInMirror) {
			this.directlyVisible = directlyVisible;
			this.visibleInMirror = visibleInMirror;
		}

		char getChar() {
			return this.name().charAt(0);
		}
	}

	private static class Position {

		int row;
		int col;
		Direction direction;

		Position(int r, int c, Direction d) {
			row = r;
			col = c;
			direction = d;
		}

		@Override
		public String toString() {
			return "Position{" + "row=" + row + ", col=" + col + ", direction=" + direction + '}';
		}
	};

	enum Direction {

		Up,
		Left,
		Down,
		Right;

		public Direction mirror(char mirrorType) {
			if (mirrorType == '/') {
				switch (this) {
					case Down:
						return Left;
					case Left:
						return Down;
					case Right:
						return Up;
					case Up:
						return Right;
				}
			} else if (mirrorType == '\\') {
				switch (this) {
					case Down:
						return Right;
					case Left:
						return Up;
					case Right:
						return Down;
					case Up:
						return Left;
				}
			}
			throw new IllegalArgumentException();
		}
	};
}
