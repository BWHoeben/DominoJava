package com.company;

import input.InputGenerator;
import javafx.util.Pair;
import objects.Board;
import objects.Coordinate;

import java.util.Scanner;
import java.util.*;

public class Main {

    public static void main(String[] args) {
        System.out.println("Available inputs: ");
        System.out.println("Option 1: ");
        new Board(InputGenerator.getInput1()).print();
        System.out.println("Option 2: ");
        new Board(InputGenerator.getInput2()).print();
        System.out.println("Option 2: ");
        new Board(InputGenerator.getInput3()).print();
        Board board = new Board(askForInput());
        System.out.println("Input: ");
        board.print();
        System.out.println();
        solve(board);
    }

    private static int[][] askForInput() {
        Scanner reader = new Scanner(System.in);
        int n = 0;
        while (n < 1 || n > 3) {
            System.out.println("Choose an input (1, 2 or 3): ");
            try {
                n = Integer.parseInt(reader.nextLine());
                if (n < 1 || n > 3) {
                    System.out.println("Invalid input.");
                }
            } catch (NumberFormatException e) {
                System.out.println("Invalid input.");
            }
        }
        reader.close();
        return InputGenerator.getInput(n);
    }

    private static void solve(Board boardToSolve) {
        Board emptyBoard = new Board(new int[8][7]);
        emptyBoard.empty();
        Set<Board> solutions = new HashSet<>();


        Set<Pair<Board, Board>> boardsToSolve = updateBoard(boardToSolve, emptyBoard);
        Set<Pair<Board, Board>> boardsToAdd = new HashSet<>();
        Set<Pair<Board, Board>> boardsToRemove = new HashSet<>();
        while (!boardsToSolve.isEmpty()) {
            for (Pair<Board, Board> boardPair : boardsToSolve) {
                if (boardPair.getKey().isEmpty()) {
                    solutions.add(boardPair.getKey());
                }
                boardsToAdd.addAll(updateBoard(boardPair.getKey(), boardPair.getValue()));
                boardsToRemove.add(boardPair);
            }
            boardsToSolve.removeAll(boardsToRemove);
            for (Pair<Board, Board> pair : boardsToAdd) {
                if (pair.getKey().isEmpty()) {
                    solutions.add(pair.getValue());
                } else {
                    boardsToSolve.add(pair);
                }
            }
            boardsToRemove.clear();
            boardsToAdd.clear();
        }
        printSolutions(solutions);
    }

    private static void printSolutions(Set<Board> solutions) {
        if (solutions.size() == 0) {
            System.out.println("This input could not be solved! :(");
        } else {
            System.out.println(solutions.size() == 1 ? "Result: " : "Results: ");
            for (Board board : solutions) {
                board.printBoneNumbers();
                System.out.println();
            }
        }
    }

    private static Set<Pair<Board, Board>> updateBoard(Board boardToSolve, Board boardToFill) {
        Map<Integer, Set<Pair<Coordinate, Coordinate>>> combinations = getCombinations(boardToSolve);
        Set<Pair<Board, Board>> boards = new HashSet<>();

        int lowestNumberOfOccurrences = 1000;
        int value = 1000;

        for (int bone : combinations.keySet()) {
            if (combinations.get(bone).isEmpty()) {
                return new HashSet<>();
            } else if (combinations.get(bone).size() == 1) {
                Set<Pair<Coordinate, Coordinate>> set = combinations.get(bone);
                for (Pair<Coordinate, Coordinate> pair : set) {
                    return getNewBoards(pair, bone, boards, boardToFill, boardToSolve);
                }
            } else if (combinations.get(bone).size() < lowestNumberOfOccurrences) {
                lowestNumberOfOccurrences = combinations.get(bone).size();
                value = bone;
            }
        }

        for (Pair<Coordinate, Coordinate> coords : combinations.get(value)) {
            boards.addAll(getNewBoards(coords, value, boards, boardToFill, boardToSolve));
        }

        return boards;
    }

    private static Board copyBoard(Board board) {
        int[][] values = cloneValues(board.getValues());
        Map<Integer, Integer> bones = cloneBones(board.getBones());
        return new Board(values, bones);
    }

    private static int[][] cloneValues(int[][] original) {
        int[][] clone = new int[8][7];
        for (int i = 0; i < original.length; i++) {
            for (int j = 0; j < original[i].length; j++)
                clone[i][j] = original[i][j];
        }
        return clone;
    }

    private static Map<Integer, Integer> cloneBones(Map<Integer, Integer> original) {
        return new HashMap<>(original);
    }

    private static Set<Pair<Board, Board>> getNewBoards(Pair<Coordinate, Coordinate> coords, int value, Set<Pair<Board, Board>> boards, Board boardToFill, Board boardToSolve) {
        boardToSolve.removeBoneWithValue(value);
        Coordinate coor1 = coords.getKey();
        Coordinate coor2 = coords.getValue();

        Board boardToFillCopy = copyBoard(boardToFill);
        boardToFillCopy.fillPosition(coor1, coor2, value);
        Board boardToSolveCopy = copyBoard(boardToSolve);
        boardToSolveCopy.clearPosition(coor1, coor2);
        Pair<Board, Board> boardPair = new Pair<>(boardToSolveCopy, boardToFillCopy);
        boards.add(boardPair);

        return boards;
    }

    private static Map<Integer, Set<Pair<Coordinate, Coordinate>>> getCombinations(Board board) {
        // This is a map with the possible positions for each combination on the board: e.g. the value for key = 34 represents all the places on the board where 3 and 4 are adjacent
        Map<Integer, Set<Pair<Coordinate, Coordinate>>> combinations = new HashMap<>();
        Map<Integer, Integer> bones = board.getBones();
        int leftPipValue;
        int rightPipValue;
        for (int i : bones.keySet()) {
            int bone = bones.get(i);
            leftPipValue = bone / 10;
            rightPipValue = bone % 10;
            Set<Pair<Coordinate, Coordinate>> possibleBonePositions = getPossibleBonePositions(leftPipValue, rightPipValue, board);
            if (leftPipValue != rightPipValue) {
                possibleBonePositions.addAll(getPossibleBonePositions(rightPipValue, leftPipValue, board));
            }
            combinations.put(bone, possibleBonePositions);
        }
        return combinations;
    }

    private static Set<Pair<Coordinate, Coordinate>> getPossibleBonePositions(int leftPipValue, int rightPipValue, Board board) {
        Set<Pair<Coordinate, Coordinate>> possibleBonePositions = new HashSet<>();

        Set<Coordinate> coordinatesOfNumber = board.getCoordinatesOfValue(leftPipValue);
        Map<Coordinate, Set<Coordinate>> coordinateAndHisNeighbours = new HashMap<>();
        for (Coordinate coor : coordinatesOfNumber) {
            coordinateAndHisNeighbours.put(coor, board.getNeighbours(coor));
        }

        for (Coordinate coor : coordinateAndHisNeighbours.keySet()) {
            Set<Coordinate> neighbours = coordinateAndHisNeighbours.get(coor);
            for (Coordinate neighbourCoor : neighbours) {
                if (board.getValue(neighbourCoor) == rightPipValue) {
                    Pair<Coordinate, Coordinate> possibleBonePos = new Pair<>(coor, neighbourCoor);
                    possibleBonePositions.add(possibleBonePos);
                }
            }
        }
        return possibleBonePositions;
    }
}
