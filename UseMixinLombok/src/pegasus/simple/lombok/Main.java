package pegasus.simple.lombok;

import lombok.Obj;
import static java.lang.System.out;
interface Animal {}

//BEGIN_PEGASUS_LOMBOK
@Obj interface Horse extends Animal {
	default void run() {out.println("running!");} }
@Obj interface Bird extends Animal {
	default void fly() {out.println("flying!");} }
@Obj interface Pegasus extends Horse, Bird {}
//END_PEGASUS_LOMBOK

public interface Main {
	public static void main(String[] args) {
		Pegasus p = Pegasus.of();
		p.run();
		p.fly();
	}

}
