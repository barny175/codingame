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

/**
 *
 * @author mbarnas
 */
public class Manor {

	private int size;
	Map<Direction, List<Integer> > canSee;
	char[][] manor;
	private final List<Equation> equations;

	public Manor(int size, Map<Direction, List<Integer> > canSee, char[][] manorTemplate) {
		equations = new ArrayList<>(4 * size);
		this.size = size;
		this.canSee = canSee;
		this.manor = manorTemplate;
		compileEquations();
	}

	public static void main(String[] args) {
		StringReader sr = new StringReader("0 3 3\n"
				+ "3\n"
				+ "2 1 2\n"
				+ "2 1 0\n"
				+ "1 2 2\n"
				+ "2 2 0\n"
				+ "../\n"
				+ "...\n"
				+ "/./");
//		Scanner in = new Scanner(System.in);
		Scanner in = new Scanner(sr);
		int vampireCount = in.nextInt();
		int zombieCount = in.nextInt();
		int ghostCount = in.nextInt();
		int size = in.nextInt();
		Map<Direction, List<Integer> > canSee = new HashMap<>();
		canSee.put(Direction.Down, readInts(size, in));
		canSee.put(Direction.Up, readInts(size, in));
		canSee.put(Direction.Right, readInts(size, in));
		canSee.put(Direction.Left, readInts(size, in));
		char[][] manorTemplate = new char[size][];
		for (int i = 0; i < size; i++) {
			manorTemplate[i] = in.nextLine().toCharArray();
		}
		
		Manor manor = new Manor(size, canSee, manorTemplate);
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
	
	Position move(Position pos, Direction direction) {
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
        Direction currDir = direction;
        Position pos = initialPos(rowOrCol, direction);
		Equation equation = new Equation();
        while (isValid(pos)) {
            equation.addElement(new EquationElement(pos.row, pos.col, mirror));
                
            if (isMirror(pos))
                mirror = true;
                
            pos = move(pos, currDir);
        }
		equation.result = result;
        return equation;
    }

	private void compileEquations() {
		this.canSee.forEach((dir, counts) -> {
			for (int i = 0; i < counts.size(); i++) {
				equations.add(compileEquation(i, Direction.Left, counts.get(i)));
			}
		});
	}
	
	private class Equation {
		int result;
		List<EquationElement> elements = new ArrayList<>();
		void addElement(EquationElement element) {
			elements.add(element);
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
			if (mirror)
				return c.visibleInMirror;
			else
				return c.directlyVisible;
		}
	}
	
	enum Creature {
		Zombie(true, true),
		Vampire(true, false),
		Ghost(false, true);
		
		boolean directlyVisible;
		boolean visibleInMirror;

		private Creature(boolean directlyVisible, boolean visibleInMirror) {
			this.directlyVisible = directlyVisible;
			this.visibleInMirror = visibleInMirror;
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
