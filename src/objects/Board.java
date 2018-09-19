package objects;

import java.util.HashMap;
import java.util.HashSet;
import java.util.Map;
import java.util.Set;

public class Board {

    private int[][] values;
    private int width = 8;
    private int height = 7;
    private Map<Integer, Integer> bones;
    private Map<Integer, Integer> bonesComplete;

    public Board(int[][] values) {
        this.values = values;
        initializeBones();
        this.bonesComplete = new HashMap<>(this.bones);
    }

    public Board(int[][] values, Map<Integer, Integer> bones) {
        this.values = values;
        this.bones = bones;
        this.bonesComplete = new HashMap<>(bones);
    }

    private void initializeBones() {
        bones = new HashMap<>();
        int index = 0;
        int j = 0;
        for (int i = 0; i < 7; i++) {
            for (int k = j; k < 7; k++) {
                index++;
                bones.put(index, (i * 10) + k);
            }
            j++;
        }
    }

    public void removeBoneWithValue(int value) {
        bones.values().remove(value);
    }

    public Set<Coordinate> getNeighbours(Coordinate coordinate) {
        int x = coordinate.getX();
        int y = coordinate.getY();

        Set<Coordinate> neighbours = new HashSet<>();
        Set<Coordinate> possibleCoors = new HashSet<>();

        // only evaluate the cell to the right and below, otherwise you'll be checking double
        possibleCoors.add(new Coordinate(x + 1, y));
        possibleCoors.add(new Coordinate(x, y + 1));

        for (Coordinate coor : possibleCoors) {
            if (isValidCoor(coor)) {
                neighbours.add(coor);
            }
        }

        return neighbours;
    }

    private boolean isValidCoor(Coordinate coordinate) {
        int x = coordinate.getX();
        int y = coordinate.getY();
        return x >= 0 && y >= 0 && x < width && y < height;
    }

    public Set<Coordinate> getCoordinatesOfValue(int val) {
        Set<Coordinate> coordinates = new HashSet<>();

        for (int i = 0; i < width; i++) {
            for (int j = 0; j < height; j++) {
                if (values[i][j] == val) {
                    coordinates.add(new Coordinate(i, j));
                }
            }
        }
        return coordinates;
    }

    public int getValue(Coordinate coordinate) {
        int x = coordinate.getX();
        int y = coordinate.getY();

        return values[x][y];
    }

    public void clearPosition(Coordinate coor1, Coordinate coor2) {
        fillPosition(coor1, coor2, -1);
    }

    public void fillPosition(Coordinate coor1, Coordinate coor2, int value) {
        this.values[coor1.getX()][coor1.getY()] = value;
        this.values[coor2.getX()][coor2.getY()] = value;
    }

    public void print() {
        for (int i = 0; i < 7; i++) {
            for (int j = 0; j < 8; j++) {
                if (values[j][i] < 0) {
                    System.out.print("   X");
                } else if (values[j][i] > 9) {
                    System.out.print("  " + values[j][i]);
                } else {
                    System.out.print("   " + values[j][i]);
                }
            }
            System.out.println();
        }
    }

    public void printBoneNumbers() {
        for (int i = 0; i < 7; i++) {
            for (int j = 0; j < 8; j++) {
                if (values[j][i] < 0) {
                    System.out.print("   X");
                } else if (valueToBoneNumber(values[j][i]) > 9) {
                    System.out.print("  " + valueToBoneNumber(values[j][i]));
                } else {
                    System.out.print("   " + valueToBoneNumber(values[j][i]));
                }
            }
            System.out.println();
        }
    }

    private int valueToBoneNumber(int value) {
        for (Integer i : bonesComplete.keySet()) {
            if (bonesComplete.get(i).equals(value)) {
                return i;
            }
        }
        return -1;
    }


    public boolean isEmpty() {
        for (int i = 0; i < 8; i++) {
            for (int j = 0; j < 7; j++) {
                if (values[i][j] != -1) {
                    return false;
                }
            }
        }

        return true;
    }

    public int[][] getValues() {
        return values;
    }

    public void empty() {
        for (int i = 0; i < 8; i++) {
            for (int j = 0; j < 7; j++) {
                values[i][j] = -1;
            }
        }
    }

    public Map<Integer, Integer> getBones() {
        return bones;
    }
}
