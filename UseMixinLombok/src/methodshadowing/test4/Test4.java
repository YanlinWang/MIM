package methodshadowing.test4;

interface D0 {
    default int m() {
        return 2;
    }
}

interface D1 {
    default int m() {
        return 1;
    }
}

interface D2 extends D0, D1 {
}

public class Test4 {
    public static void main(String[] args) {

    }
}
