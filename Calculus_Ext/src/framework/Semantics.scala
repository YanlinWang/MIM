package framework

import AST._

class Semantics(info: Info, expr: Expr) {
  val eval: Expr = eval(expr)
  
  case object NoRuleApplies extends Throwable
  case object Buggy extends Throwable
  
  def isVal(e: Expr): Boolean = e match {
    case AnnoExpr(_, New(_)) => true
    case _                   => false
  }
  
  def eval1(e: Expr): Expr = e match {
    case New(i) => AnnoExpr(i, New(i))
    case Invk(AnnoExpr(j, New(i)), m, args) => {
      for (index <- 0 to args.length - 1) {
        val arg = args(index)
        if (!isVal(arg)) return Invk(AnnoExpr(j, New(i)), m, args.updated(index, eval1(arg)))
      }
      val mbody = info.mbody(m, i, j).get
      val i0 = mbody._1
      val params = mbody._2
      val ie = mbody._3._1
      val e0 = mbody._3._2.get
      val map = params.zip(args).map(p => p._1._2 -> AnnoExpr( p._1._1, p._2)).toMap + ("this" -> AnnoExpr(i0, New(i)))
      AnnoExpr(ie, e0.subst(map))
    }
    case Invk(e0, m, args) => Invk(eval1(e0), m, args)
    case AnnoExpr(i, AnnoExpr(j, New(k))) => AnnoExpr(i, New(k))
    case AnnoExpr(_, New(_)) => throw NoRuleApplies
    case AnnoExpr(i, e0) => AnnoExpr(i, eval1(e0))
    case Var(_) => throw Buggy
    case _ => throw NoRuleApplies
  }
  
  def eval(e: Expr): Expr = {
    try {println("==> " + Pretty.pretty(e)); eval(eval1(e))} catch {
      case NoRuleApplies => e
      case Buggy         => throw new Exception("Wrong evaluation.")
    }
  }
}