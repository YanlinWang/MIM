package test;

import static java.lang.System.out;
import lombok.Obj;

//@Obj
interface Point2D {
    int x();
    public static Point2D of(int x) {
        return new Point2D() {
            int _x = x;
            public int x() { return _x; }
        };
    }
}

public interface TestYanlin {
    public static void main(String[] args) {
        Point2D p = Point2D.of(3);
        out.println(p.x());
    }
}
