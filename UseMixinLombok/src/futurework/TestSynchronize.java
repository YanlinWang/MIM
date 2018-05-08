package futurework;

/*
 \yanlin{ what would happen if Classless Java is used in parallel programming
setting. Clearly interfaces (even with our annotation) cannot model everything
classes support, especially in a parallel setting, the use of Java keyword
`synchronized` is limited inside interfaces. One of the limitation is that
`synchronized` can’t be applied on a method inside interface as in classes (This
is true for both default and static methods).

One workaround is: instead of synchronize on a whole method, we use
synchronized block on `this` (see interface B). But whether this workaround is
correct still needs experiment. I think this maybe another limitation of our
approach. }
 */


class A {
    Integer x;
    synchronized void print() {
        synchronized (x) {
            x = x + 1;
            System.out.println(x);
        }
    }
}

interface B {
    Integer x();
    void x(Integer x);
    /*synchronized*/ default void print() {
        synchronized (this) {
            x(x() + 1);
            System.out.println(x());
        }
    }
    /*synchronized*/ static void print2() {
    }
}

public interface TestSynchronize {

}