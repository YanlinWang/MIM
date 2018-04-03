package framework

import Configuration._

object Error {
  
  case object Buggy extends Throwable
  case object TODO extends Throwable
  case class Done(c: Config) extends Throwable
  
}