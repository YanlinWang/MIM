package framework

import scala.io.Source
import AST._
import CompilationError._

object Main {
  def run(fileName: String): Unit = {
    val program = Source.fromFile("examples/" + fileName + ".fhj").mkString
    val parsed = FHJParser.parse(program)
    if (parsed.isLeft) println(parsed.left.get)
    else {
      val p: Program = parsed.right.get
      println(Pretty.pretty(p) + "\n")
      try {
        val info = Preprocessor.createInfo(p)
        val typeCheck = p.programCheck(info)
        val eval = new Semantics(info, p.e).eval
      } catch {case error: Throwable => printError(error)}
    }
  }               
  
  def main(args : Array[String]) = {
    println("----------------------------------\nExample (a):\n")
    run("Example_a")  // <A> new A()
    println("----------------------------------\nExample (b):\n")
    run("Example_b")  // <A> new C()
    println("----------------------------------\nExample (c):\n")
    run("Example_c")  // <A> new C()
    println("----------------------------------\nExample (d):\n")
    run("Example_d")  // Type-check: Interface check failed
    println("----------------------------------\nExample (e):\n")
    run("Example_e")  // Type-check: Interface check failed
    println("----------------------------------\nExample (f):\n")
    run("Example_f")  // <T> new C()
    println("----------------------------------\nExample (Problem 1):\n")
    run("Example_p1") // <Void> new FromDeck()
    println("----------------------------------\nExample (Problem 2):\n")
    run("Example_p2") // <Void> new FromSafeDeck()
    println("----------------------------------\nExample (Problem 3):\n")
    run("Example_p3") // <Void> new FromDrawableSafeDeck()
    println("----------------------------------\nExample (g):\n")
    run("Example_g")  // ((T) new FromAB())
    println("----------------------------------")
  }
  
}