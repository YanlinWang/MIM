package framework

import scala.io.Source
import AST._

object Main {
  def run(fileName: String): Unit = {
    val program = Source.fromFile("examples/" + fileName + ".state").mkString
    val parsed = StateMIMParser.parse(program)
    if (parsed.isLeft) println(parsed.left.get)
    else {
      val p: Program = parsed.right.get
      val pretty = p.toString
      println(pretty + "\n")
        val info: Info = p.collectInfo.get
      try {
        val typeCheck = p.programCheck(info)
        println("Type check: ==> " + typeCheck)
        val eval = parsed.right.get.eval()
        println(eval)
      } catch {case error: Throwable => Error.printError(error)}
      
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
//    println("----------------------------------\nTest6:\n")
//    run("Test6")
//    println("----------------------------------\nTest7:\n")
//    run("Test7")
    println("----------------------------------\nTest8:\n")
    run("Test8")
  }
}