package com.company;

import input.InputGenerator;
import javafx.util.Pair;
import objects.Board;
import objects.Coordinate;

import java.util.*;

public class Main {

    public static void main(String[] args) {
        Board board = new Board(new InputGenerator().getInput1());
        solve(board);
    }

    private static void solve(Board boardToSolve) {
        Board emptyBoard = new Board(new int[8][7]);
        Set<Board> solutions = new HashSet<>();


        Set<Pair<Board, Board>> boardsToSolve = updateBoard(boardToSolve, emptyBoard);

        while (boardsToSolve.size() > 0) {
            for (Pair<Board, Board> boardPair : boardsToSolve) {
                if (boardPair.getKey().isEmpty()) {
                    solutions.add(boardPair.getKey());
                }
                Set<Pair<Board, Board>> newBoards = updateBoard(boardPair.getKey(), boardPair.getValue());
                boardsToSolve.addAll(newBoards);
                boardsToSolve.remove(boardPair);
            }
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

        int lowestNumberOfOccurences = 1000;
        int value = 1000;

        for (int bone : combinations.keySet()) {
            if (combinations.get(bone).size() == 1) {
                Set<Pair<Coordinate, Coordinate>> set = combinations.get(bone);
                for (Pair<Coordinate, Coordinate> pair : set) {
                    return getNewBoards(pair, bone, boards, boardToFill, boardToSolve);
                }
            } else if (combinations.get(bone).size() < lowestNumberOfOccurences) {
                lowestNumberOfOccurences = combinations.get(bone).size();
                value = bone;
            }
        }

        for (Pair<Coordinate, Coordinate> coords : combinations.get(value)) {
            boards.addAll(getNewBoards(coords, value, boards, boardToFill, boardToSolve));
        }

        return boards;
    }

    private static Set<Pair<Board, Board>> getNewBoards(Pair<Coordinate, Coordinate> coords, int value, Set<Pair<Board, Board>> boards, Board boardToFill, Board boardToSolve) {
        Coordinate coor1 = coords.getKey();
        Coordinate coor2 = coords.getValue();
        boardToFill.fillPosition(coor1, coor2, value);
        boardToSolve.clearPosition(coor1, coor2);
        Pair<Board, Board> boardPair = new Pair<>(boardToSolve, boardToFill);
        boards.add(boardPair);
        return boards;
    }

    private static Map<Integer, Set<Pair<Coordinate, Coordinate>>> getCombinations(Board board) {
        // This is a map with the possible positions for each combination on the board: e.g. the value for key = 34 represents all the places on the board where 3 and 4 are adjacent
        Map<Integer, Set<Pair<Coordinate, Coordinate>>> combinations = new HashMap<>();
        Map<Integer, Integer> bones = getBones();
        int leftPipValue;
        int rightPipValue;
        for (int i = 0; i < bones.size(); i++) {
            int bone = bones.get(i + 1);
            leftPipValue = bone / 10;
            rightPipValue = bone % 10;
            Set<Pair<Coordinate, Coordinate>> possibleBonePositions = getPossibleBonePositions(leftPipValue, rightPipValue, board);
            combinations.put(bone, possibleBonePositions);
        }
        return combinations;
    }

    private static Map<Integer, Integer> getBones() {
        Map<Integer, Integer> bones = new HashMap<>();
        int index = 0;
        int j = 0;
        for (int i = 0; i < 7; i++) {
            for (int k = j; k < 7; k++) {
                index++;
                bones.put(index, (i * 10) + k);
            }
            j++;
        }
        return bones;
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
