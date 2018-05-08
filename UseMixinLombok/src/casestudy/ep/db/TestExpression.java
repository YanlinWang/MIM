package casestudy.ep.db;

import lombok.Obj;

public class TestExpression {
    public static void main(String[] args) {
        ExpD e1 = AddD.of(LitD.of(3), LitD.of(4));
        e1.db();
        System.out.println(e1.eval()); //expecting: 14
    }
}

interface Exp { int eval(); }
@Obj interface Lit extends Exp {
    int x();
    default int eval() {return x();}
}
@Obj interface Add extends Exp {
    Exp e1(); Exp e2();
    default int eval() {
        return e1().eval() + e2().eval();
    }
}

interface ExpD extends Exp { void db(); }
@Obj interface LitD extends ExpD, Lit {
    void x(int val);
    default void db() { x(2*x()); } 
}
@Obj interface AddD extends ExpD, Add {
    ExpD e1(); ExpD e2();
    default void db() { e1().db(); e2().db(); }
}