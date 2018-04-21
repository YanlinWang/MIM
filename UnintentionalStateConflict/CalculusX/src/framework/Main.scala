package framework

import scala.io.Source

object Main {
  def run(fileName: String): Unit = {
    val program = Source.fromFile("examples/" + fileName + ".fhj").mkString
    val parsed = FHJParser.parse(program)
    if (parsed.isLeft) println(parsed.left.get)
    else {
      val pretty = parsed.right.get.toString
      println(pretty + "\n")
      val eval = parsed.right.get.run
      if (eval.isLeft) println("Type-check: " + eval.left.get)
      else println("==> " + eval.right.get.toString)
    }
  }               
  
  def main(args : Array[String]) = {
//    println("----------------------------------\nExample (a):\n")
//    run("Example_a")
//    println("----------------------------------\nExample (b):\n")
//    run("Example_b")
//    println("----------------------------------\nExample (c):\n")
//    run("Example_c")
//    println("----------------------------------\nExample (d):\n")
//    run("Example_d")
//    println("----------------------------------\nExample (e):\n")
//    run("Example_e")
//    println("----------------------------------\nExample (f):\n")
//    run("Example_f")
//    println("----------------------------------\nExample (Problem 1):\n")
//    run("Example_p1")
//    println("----------------------------------\nExample (Problem 2):\n")
//    run("Example_p2")
//    println("----------------------------------\nExample (Problem 3):\n")
//    run("Example_p3")
//    println("----------------------------------")
    println("----------------------------------\nExample (x):\n")
    run("Example_x")
  }
  
}