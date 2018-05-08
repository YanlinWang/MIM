package methodshadowing.test2;

interface D0 {
    int m();
}

interface D1 {
    default int m() {
        return 1;
    }
}

interface D2 extends D0, D1 {
    //conflicted m()
}

public class Test2 {
}
