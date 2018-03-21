package framework

import AST._

object CompilationError {
  case class TypeError(t: String) extends Throwable
  case class MethError(m: MethDef) extends Throwable
  case class TypeNotFound(t: String) extends Throwable
  case class VarNotInScope(x: String) extends Throwable
  case class Message(s: String) extends Throwable
  
  def assert[A](f: Boolean, exception: Throwable): Unit = if (!f) throw exception
  def nextError(f: Throwable, g: Throwable): Unit = {printError(f); throw g}
  def printError(f: Throwable): Unit = f match {
    case TypeNotFound(t)  => println("Type " + t + " not found.")
    case VarNotInScope(x) => println("Variable " + x + " not in scope.")
    case Message(s)       => println("Error: " + s)
    case MethError(m)     => println("MethCheck failed: " + Pretty.pretty(m))
    case TypeError(t)     => println("InterfaceCheck failed: " + t)
  }
}