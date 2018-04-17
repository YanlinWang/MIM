package framework

import scala.io.Source

object Main {
  def run(fileName: String): Unit = {
    val program = Source.fromFile("examples/" + fileName + ".state").mkString
    val parsed = StateMIMParser.parse(program)
    if (parsed.isLeft) println(parsed.left.get)
    else {
      val pretty = parsed.right.get.toString
      println(pretty + "\n")
      val eval = parsed.right.get.eval()
      println(eval)
    }
  }     
  def main(args : Array[String]) = {    
//    println("----------------------------------\nTest0:\n")
//    run("Test0")
//    println("----------------------------------\nTest1:\n")
//    run("Test1")
//    println("----------------------------------\nTest2:\n")
//    run("Test2")
//    println("----------------------------------\nTest3:\n")
//    run("Test3")
//    println("----------------------------------\nTest4:\n")
//    run("Test4")
//    println("----------------------------------\nTest5:\n")
//    run("Test5")
    run("Test6")
  }
}