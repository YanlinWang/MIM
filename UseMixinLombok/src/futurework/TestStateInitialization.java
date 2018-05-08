package futurework;

import static java.lang.System.out;

//BEGIN_STATE_INIT
interface Box { 
    default int val() { return 0; } //provided
    void val(int _val); //provided
    static Box of() { return new Box() { //generated
        int val = Box.super.val();
        public int val() { return val; }
        public void val(int _val) { val = _val; }
    };} }
//END_STATE_INIT

interface X {
    /** provided **/
    //@Default(3)
    int val();
    X val(int val);
    /** generated **/
    static X of() { return new X() {
        int _val = 3;
        public int val() { return _val; }
        public X val(int val) { _val = val; return this; }
    };}
}

public class TestStateInitialization {
    public static void main(String[] args) {
        out.println(new A4(){}.val());
    }
}

interface A1 { int val(); }
interface A2 extends A1 { default int val() { return 3; } }
interface A3 extends A1 { default int val() { return 4; } }
interface A4 extends A2, A3 {}

interface B1 { 
    //@Default(5)
    int val();
}
interface B2 extends B1 {}
interface B3 extends B1 {}
interface B4 extends B2, B3 {}