package futurework;

import lombok.Obj;


//now mixin generate clone always. This prevent the user 
//to define its own
/*public @Mixin interface CloneTest{
  int foo();
  default CloneTest clone(){ return of(2);}
  public static void main(String[] arg){
    CloneTest t=CloneTest.of(3);
    t=t.clone();
    System.out.println(t.foo());
  } 
}*/


//@Obj 
interface Y {
    /** provided **/
    int x();
    default Y clone() { return Y.of(0); } //user defined clone
    /** generated **/
    public static Y of(int _x) {
        return new Y() {
            int _x;
            public Y clone() { return Y.super.clone();}
            public int x() { return _x; }
        };
    } 
}

//@Obj
interface Z {
    /** provided **/
    int x();
    Z clone();
    /** generated **/
    public static Y of(int _x) {
        return new Y() {
            int _x;
            public Y clone() { return of(x()); }
            public int x() { return _x; }
        };
    }
}


public interface TestClone{
    int foo();
    public static TestClone of(int val){
        return new TestClone(){
            public int foo(){return val;}
            //public CloneTest clone(){//This is what we should generate. it look sort of sad and hard to explain...
            //  return CloneTest.super.clone();}
            //we may want to do similar thing for toString.... may be we just write it in future work?
        };
    }
    default TestClone clone(){ return of(2);}
    public static void main(String[] arg){
        TestClone t=TestClone.of(3);
        t=t.clone();
        System.out.println(t.foo());
    } 
}