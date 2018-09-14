package com.company;

import input.InputGenerator;
import javafx.util.Pair;
import objects.Board;
import objects.Coordinate;

import java.util.*;

public class Main {

    public static void main(String[] args) {
        Board board = new Board(new InputGenerator().getInput1());
        board.print();
        solve(board);
    }

    private static void solve(Board boardToSolve) {
        Board emptyBoard = new Board(new int[8][7]);
        emptyBoard.empty();
        Set<Board> solutions = new HashSet<>();


        Set<Pair<Board, Board>> boardsToSolve = updateBoard(boardToSolve, emptyBoard);
        Set<Pair<Board, Board>> boardsToAdd = new HashSet<>();
        Set<Pair<Board, Board>> boardsToRemove = new HashSet<>();
        while (boardsToSolve.size() > 0) {
            for (Pair<Board, Board> boardPair : boardsToSolve) {
                if (boardPair.getKey().isEmpty()) {
                    solutions.add(boardPair.getKey());
                }
                boardsToAdd.addAll(updateBoard(boardPair.getKey(), boardPair.getValue()));
                boardsToRemove.add(boardPair);
            }
            boardsToSolve.removeAll(boardsToRemove);
            boardsToSolve.addAll(boardsToAdd);
            boardsToRemove.clear();
            boardsToAdd.clear();
            System.out.println(boardsToSolve.size());
        }
        printSolutions(solutions);
    }

    private static void printSolutions(Set<Board> solutions) {
        if (solutions.size() == 0) {
            System.out.println("This input could not be solved! :(");
        }

        for (Board board : solutions) {
            System.out.println("Solution: ");
            board.print();
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
        return new Board(board.getValues(), board.getBones());
    }

    private static Set<Pair<Board, Board>> getNewBoards(Pair<Coordinate, Coordinate> coords, int value, Set<Pair<Board, Board>> boards, Board boardToFill, Board boardToSolve) {
        boardToSolve.removeBoneWithValue(value);
        System.out.println("Removing bone: " + (value));
        Coordinate coor1 = coords.getKey();
        Coordinate coor2 = coords.getValue();
        Board boardToFillCopy = copyBoard(boardToFill);
        boardToFillCopy.fillPosition(coor1, coor2, value);
        Board boardToSolveCopy = copyBoard(boardToSolve);
        boardToSolveCopy.clearPosition(coor1, coor2);
        Pair<Board, Board> boardPair = new Pair<>(boardToSolveCopy, boardToFillCopy);
        boards.add(boardPair);

        System.out.println("Board to fill:");
        boardToFill.print();

        System.out.println("Board to solve:");
        boardToSolve.print();

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
