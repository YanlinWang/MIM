package methodshadowing.test3;

interface D0 {
    default int m() {
        return 2;
    }
}

interface D1 extends D0 {
    default int m() {
        return 1;
    }
}

interface D2 extends D0, D1 {
    default int n() {
        return m(); //eclipse says D0.m(), but it's actually D1.m();
    }
}

public class Test3 {
    public static void main(String[] args) {
        D2 d2 = new D2() {

        };

        System.out.println(d2.m());
    }
}
