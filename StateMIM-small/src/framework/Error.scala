package framework

import AST._

object Error {
  
  case object Buggy extends Throwable
  case object TODO extends Throwable
  case class Done(v: Value) extends Throwable
  
}