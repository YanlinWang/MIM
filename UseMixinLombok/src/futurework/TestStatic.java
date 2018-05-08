package futurework;

public interface TestStatic {
    int foo();
    class Foo{private static TestStatic of(int foo){
        return new TestStatic(){public int foo(){return foo;}};
    }}
    static TestStatic of(int foo){
        if(foo<0)foo=-foo;
        return Foo.of(foo);
    }

}

interface StaticSee extends TestStatic{
    static TestStatic boom(){
        return TestStatic.Foo.of(-2);
    }
}