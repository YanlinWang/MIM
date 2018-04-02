package framework

import Configuration._
import Error._
import AST._

object Semantics {
  
  def eval1(config: Config): Config = {
    val h = config.h
    val vs = config.vs
    val stack = config.stack
    if (config.expr.isEmpty) {
      stack.head match {
        case Right(_) => throw Buggy
        case Left(Var(x)) => throw TODO
        case Left(Invk(e, m, args)) => throw TODO
        case Left(InvkStatic(t, m, args)) => throw TODO
        case Left(AnnoExpr(i, e)) => throw TODO
        case Left(InvkSetter(e, fieldName, para)) => throw TODO
        case Left(LetExpr(t, x, e1, e2)) => throw TODO
        case Left(Value(t, id)) => throw TODO
        case _ => throw Buggy
      }
    } else config.expr.get match {
      case Var(x) => throw TODO
      case Invk(e, m, args) => throw TODO
      case InvkStatic(t, m, args) => throw TODO
      case AnnoExpr(i, e) => throw TODO
      case InvkSetter(e, fieldName, para) => throw TODO
      case LetExpr(t, x, e1, e2) => throw TODO
      case Value(t, id) => throw TODO
    }
  }
  
}