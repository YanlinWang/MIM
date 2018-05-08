package casestudy.ep.mutability;

import java.util.ArrayList;
import java.util.List;
import lombok.Obj;

public class TestExpression {
    public static String runTest() {
        String res = "";
        Lit e1 = Lit.of(3);
        res += e1.eval() + "\n"; // 3
        Lit e2 = Lit.of(4);
        Add e3 = Add.of(e1, e2);
        res += e3.eval() + "\n"; // 7
        Sub e4 = Sub.of(e1, e2);
        res += e4.eval() + "\n"; // -1
        LitP e5 = LitP.of(3);
        LitP e6 = LitP.of(4);
        AddP e7 = AddP.of(e5, e6);
        res += e5.print() + " = " + e5.eval() + "\n"; // 3 = 3
        res += e7.print() + " = " + e7.eval() + "\n"; // (3 + 4) = 7
        return res;
    }
    public static void testMutability() {
        System.out.println(">>>>>>>> Testing Mutability");
        AddP e1 = AddP.of(LitP.of(3), LitP.of(4));
        e1.e2(LitP.of(5));
        System.out.println(e1.print());
        
        AddPC e2 = AddPC.of(LitPC.of(3), LitPC.of(4));
        e2.e2(LitPC.of(7));
        System.out.println(e2.print());
    }
    public static void main(String[] args) {
        System.out.println(runTest());
        testMutability();
        //independent extensibility
        System.out.println(">>>>>>>> Testing Independent Extensibility");
        ExpPC e8 = AddPC.of(LitPC.of(3), LitPC.of(4));
        System.out.println(e8.print() + " = " + e8.eval() + " Literals: " + e8.collectLit().toString());
    }
}

//BEGIN_EXPRESSION_INIT2
interface Exp { int eval(); Exp with(Exp val);}
@Obj interface Lit extends Exp {
    int x(); void x(int val);
    default int eval() {return x();}
}
@Obj interface Add extends Exp {
    Exp e1(); Exp e2(); void e1(Exp val); void e2(Exp val);
    default int eval() {
        return e1().eval() + e2().eval();
    }
}
//END_EXPRESSION_INIT2

//BEGIN_EXPRESSION_SUB2
@Obj interface Sub extends Exp {
	Exp e1(); Exp e2();
	default int eval() {
		return e1().eval() - e2().eval();} }
//END_EXPRESSION_SUB2

//BEGIN_EXPRESSION_PRINT2
interface ExpP extends Exp {String print();}
@Obj interface LitP extends Lit, ExpP {
    default String print() {return "" + x();}
}
@Obj interface AddP extends Add, ExpP {
    ExpP e1(); //return type refined! 
    ExpP e2(); //return type refined!
    default String print() {
        return "(" + e1().print() + " + " 
                + e2().print() + ")";}
    void e1(ExpP val); void e2(ExpP val);
    default void e1(Exp val) { e1(e1().with(val)); }
    default void e2(Exp val) { e2(e2().with(val)); }
}
//END_EXPRESSION_PRINT2

//BEGIN_EXPRESSION_COLLECTLIT2
interface ExpC extends Exp { List<Integer> collectLit(); }
@Obj interface LitC extends Lit, ExpC {
    default List<Integer> collectLit() {
        List<Integer> list = new ArrayList<Integer>();
        list.add(x());
        return list;
    }
}
@Obj interface AddC extends Add, ExpC {
    ExpC e1(); ExpC e2();
    default List<Integer> collectLit() {
        List<Integer> list = new ArrayList<Integer>();
        list.addAll(e1().collectLit());
        list.addAll(e2().collectLit());
        return list;
    }
    void e1(ExpC val); void e2(ExpC val);
    default void e1(Exp val) { e1(e1().with(val)); }
    default void e2(Exp val) { e2(e2().with(val)); }
}
//END_EXPRESSION_COLLECTLIT2

//BEGIN_INDEPENDENT_EXTENSIBILITY2
interface ExpPC extends ExpP, ExpC {
    ExpPC with(Exp val);
}
@Obj interface LitPC extends ExpPC, LitP, LitC { LitPC with(Exp val); }
@Obj interface AddPC extends ExpPC, AddP, AddC { ExpPC e1(); ExpPC e2();
    AddPC with(Exp val);
    void e1(ExpPC val);
    void e2(ExpPC val); 
    default void e1(Exp val) { e1(e1().with(val)); }
    default void e2(Exp val) { e2(e2().with(val)); }
    default void e1(ExpP val) { e1(e1().with(val)); }
    default void e1(ExpC val) { e1(e1().with(val)); }
    default void e2(ExpP val) { e2(e2().with(val)); }
    default void e2(ExpC val) { e2(e2().with(val)); }
}
//END_INDEPENDENT_EXTENSIBILITY2
