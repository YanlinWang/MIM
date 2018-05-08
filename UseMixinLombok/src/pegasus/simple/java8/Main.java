package pegasus.simple.java8;

import static java.lang.System.out;

//BEGIN_PEGASUS_JAVA
interface Animal {} // no points yet!
interface Horse extends Animal {
    default void run(){out.println("run!");}
}
interface Bird extends Animal {
    default void fly(){out.println("fly!");}
}
interface Pegasus extends Horse, Bird {}
//END_PEGASUS_JAVA

//BEGIN_PEGASUS_INST
class HorseImpl implements Horse {}
class BirdImpl implements Bird {} 
class PegasusImpl implements Pegasus {}
//END_PEGASUS_INST

public interface Main {
    public static void main(String[] args) {
        Horse h = new HorseImpl();
        Bird b = new BirdImpl();
        Pegasus p = new PegasusImpl();	
    }
}
