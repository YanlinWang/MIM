package methodshadowing.test1;


interface A1 { Object m(); }
interface B1 { Integer m();}
interface C1 extends A1,B1 {} //accepted

interface A2 { default int m() {return 1;}}
interface B2 { int m(); }
interface C2 { default int m() {return 2;}}
interface D2 extends A2,B2 {} //rejected due to conflicting methods
interface E2 extends A2,C2 {} //rejected due to conflicting methods

interface D3 extends A2,B2 { int m(); } //accepted
interface E3 extends A2,C2 { int m(); } //accepted

interface X0 {
    int m();
}

interface X1 extends X0 {
    default int m() {
        return 1;
    }
}

interface X2 extends X0, X1 {
    default int n() {
        return m();
    }
}

public class Test1 {
    public static void main(String[] args) {
        X2 d2 = new X2() {

        };

        System.out.println(d2.m());
    }
}
