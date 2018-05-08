package test;

import static org.junit.Assert.assertEquals;
import org.junit.Test;
import pegasus.TestAnimal;
import casestudy.ep.TestExpression;

public class AutoTest {
	@Test
	public void testExpression() {
		String s = "3" + "\n";
		s += "7" + "\n";
		s += "-1" + "\n";
		s += "3 = 3" + "\n";
		s += "(3 + 4) = 7" + "\n";
		assertEquals(s, TestExpression.runTest());
	}

	@Test
	public void testAnimal() {
		String s = "horse initialized at 42, 0" + "\n";
		s += "horse.run(); now at 62, 0" + "\n";
		s += "\n";
		s += "bird initialized at 10, 10, 10" + "\n";
		s += "bird.fly(); now at 50, 10, 10" + "\n";
		s += "\n";
		s += "pegasus initialized at 100, 100, 100" + "\n";
		s += "pegasus.run(); now at 120, 100, 100" + "\n";
		s += "pegasus.fly(); now at 160, 100, 100" + "\n";
		assertEquals(s, TestAnimal.runTest());
	}
}
