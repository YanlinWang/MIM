package test;

import java.util.List;


interface ImmuList {
    List<Integer> list();
}
interface ImmuListLen extends ImmuList {
    int len();
}

public interface Test {

}
