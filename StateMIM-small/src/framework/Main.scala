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
//    run("Test0") // (type = A,res = A[])
//    println("----------------------------------\nTest1:\n")
//    run("Test1") // (type = A,res = A[])
//    println("----------------------------------\nTest2:\n")
//    run("Test2") // (type = Int,res = 3)
//    println("----------------------------------\nTest3:\n")
//    run("Test3") // (type = Int,res = 2)
//    println("----------------------------------\nTest4:\n")
//    run("Test4") // (type = Int,res = 2)
//    println("----------------------------------\nTest5:\n")
//    run("Test5") // (type = Int,res = 3)
//    println("----------------------------------\nTest6:\n")
//    run("Test6") // (type = OfficeClerk,res = OfficeClerk["Alice", 27, Officer[], 2, Point[2, 4]])
//    println("----------------------------------\nTest7:\n")
//    run("Test7") // (type = String,res = "abc")
//    println("----------------------------------\nTest8:\n")
//    run("Test8") // (type = C,res = C[1])
//    println("----------------------------------\nTest9:\n")
//    run("Test9") // (type = B,obj = B[I1[]])
//    println("----------------------------------\nTest10:\n")
//    run("Test10") //
//    println("----------------------------------\nTest11:\n")
//    run("Test11") //
    println("----------------------------------\nTest12:\n")
    run("Test12") //
    
  }
}