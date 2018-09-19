package input;

public class InputGenerator {

    public InputGenerator() {

    }

    public static int[][] getInput(int num) {
        if (num == 1) {
            return getInput1();
        } else if (num == 2) {
            return getInput2();
        } else if (num == 3) {
            return getInput3();
        }
        return new int [0][0];
    }

    public static int[][] getInput1() {
        int[][] input = new int[8][7];
        input[0][0] = 4;
        input[0][1] = 5;
        input[0][2] = 1;
        input[0][3] = 1;
        input[0][4] = 4;
        input[0][5] = 4;
        input[0][6] = 6;

        input[1][0] = 2;
        input[1][1] = 0;
        input[1][2] = 2;
        input[1][3] = 4;
        input[1][4] = 0;
        input[1][5] = 0;
        input[1][6] = 5;

        input[2][0] = 5;
        input[2][1] = 4;
        input[2][2] = 3;
        input[2][3] = 0;
        input[2][4] = 6;
        input[2][5] = 1;
        input[2][6] = 3;

        input[3][0] = 2;
        input[3][1] = 3;
        input[3][2] = 0;
        input[3][3] = 1;
        input[3][4] = 0;
        input[3][5] = 6;
        input[3][6] = 6;

        input[4][0] = 6;
        input[4][1] = 1;
        input[4][2] = 2;
        input[4][3] = 3;
        input[4][4] = 3;
        input[4][5] = 4;
        input[4][6] = 2;

        input[5][0] = 3;
        input[5][1] = 4;
        input[5][2] = 2;
        input[5][3] = 5;
        input[5][4] = 6;
        input[5][5] = 0;
        input[5][6] = 1;

        input[6][0] = 5;
        input[6][1] = 1;
        input[6][2] = 2;
        input[6][3] = 6;
        input[6][4] = 6;
        input[6][5] = 3;
        input[6][6] = 5;

        input[7][0] = 4;
        input[7][1] = 1;
        input[7][2] = 2;
        input[7][3] = 5;
        input[7][4] = 5;
        input[7][5] = 0;
        input[7][6] = 3;

        return input;
    }

    public static int[][] getInput2() {
        int[][] input = new int[8][7];
        input[0][0] = 5;
        input[0][1] = 0;
        input[0][2] = 3;
        input[0][3] = 5;
        input[0][4] = 4;
        input[0][5] = 5;
        input[0][6] = 5;

        input[1][0] = 4;
        input[1][1] = 6;
        input[1][2] = 2;
        input[1][3] = 3;
        input[1][4] = 0;
        input[1][5] = 2;
        input[1][6] = 5;

        input[2][0] = 3;
        input[2][1] = 0;
        input[2][2] = 6;
        input[2][3] = 6;
        input[2][4] = 4;
        input[2][5] = 2;
        input[2][6] = 3;

        input[3][0] = 6;
        input[3][1] = 1;
        input[3][2] = 5;
        input[3][3] = 2;
        input[3][4] = 1;
        input[3][5] = 4;
        input[3][6] = 6;

        input[4][0] = 5;
        input[4][1] = 2;
        input[4][2] = 0;
        input[4][3] = 3;
        input[4][4] = 0;
        input[4][5] = 4;
        input[4][6] = 1;

        input[5][0] = 3;
        input[5][1] = 3;
        input[5][2] = 4;
        input[5][3] = 2;
        input[5][4] = 0;
        input[5][5] = 1;
        input[5][6] = 2;

        input[6][0] = 4;
        input[6][1] = 1;
        input[6][2] = 2;
        input[6][3] = 0;
        input[6][4] = 4;
        input[6][5] = 6;
        input[6][6] = 3;

        input[7][0] = 6;
        input[7][1] = 1;
        input[7][2] = 0;
        input[7][3] = 6;
        input[7][4] = 1;
        input[7][5] = 5;
        input[7][6] = 1;

        return input;
    }

    public static int[][] getInput3() {
        int[][] input = new int[8][7];
        input[0][0] = 6;
        input[0][1] = 1;
        input[0][2] = 1;
        input[0][3] = 1;
        input[0][4] = 5;
        input[0][5] = 5;
        input[0][6] = 6;

        input[1][0] = 6;
        input[1][1] = 3;
        input[1][2] = 3;
        input[1][3] = 0;
        input[1][4] = 1;
        input[1][5] = 5;
        input[1][6] = 0;

        input[2][0] = 2;
        input[2][1] = 2;
        input[2][2] = 2;
        input[2][3] = 4;
        input[2][4] = 3;
        input[2][5] = 4;
        input[2][6] = 5;

        input[3][0] = 6;
        input[3][1] = 0;
        input[3][2] = 4;
        input[3][3] = 3;
        input[3][4] = 6;
        input[3][5] = 0;
        input[3][6] = 3;

        input[4][0] = 5;
        input[4][1] = 1;
        input[4][2] = 6;
        input[4][3] = 2;
        input[4][4] = 0;
        input[4][5] = 2;
        input[4][6] = 4;

        input[5][0] = 2;
        input[5][1] = 0;
        input[5][2] = 6;
        input[5][3] = 1;
        input[5][4] = 4;
        input[5][5] = 6;
        input[5][6] = 2;

        input[6][0] = 4;
        input[6][1] = 3;
        input[6][2] = 5;
        input[6][3] = 1;
        input[6][4] = 5;
        input[6][5] = 0;
        input[6][6] = 0;

        input[7][0] = 1;
        input[7][1] = 4;
        input[7][2] = 4;
        input[7][3] = 2;
        input[7][4] = 5;
        input[7][5] = 3;
        input[7][6] = 3;

        return input;
    }


}
