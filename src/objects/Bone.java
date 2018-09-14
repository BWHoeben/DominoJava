package objects;

public class Bone {

    private int number;
    private Pip leftPip = new Pip();
    private Pip rightPip = new Pip();

    public Bone () {
        this.number = number;
        this.leftPip = leftPip;
        this.rightPip = rightPip;
    }

    public Pip getLeftPip() {
        return leftPip;
    }

    public Pip getRightPip() {
        return rightPip;
    }
}
