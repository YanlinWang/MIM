package methodshadowing.test5;

//BEGIN_JAVABUG
interface A1 { int m(); }
interface B1 { default int m() { return 0; }}
interface B2 extends B1 { int m(); }
interface B3 extends B2 { default int m() {return 1;}}
interface M1 extends A1, B3 {} //accepted, but should be rejected
interface M2 extends A1, B1 {} //rejected
//END_JAVABUG

public class Test5 {
    public static void main(String[] args) {
        M1 d2 = new M1() {

        };

        System.out.println(d2.m());
    }
}
