package framework

import Configuration._
import Error._
import AST._

object Semantics {
  
  def eval1(info: Info, config: Config): Config = {
    val h = config.h
    config.e match {
      case Var(x) => {
        println(x)
        throw Buggy
      }
      case Invk(e, m, args) => {
        if (!e.isValue) return eval1(info, Config(h, e)).update(Invk(_, m, args))
        for (index <- 0 to args.length - 1) {
          if (!args(index).isValue)
            return eval1(info, Config(h, args(index))).update(x => Invk(e, m, args.updated(index, x)))
        }
        
        val eValue = e match { case v : Value => v } //cast e to value
        val j = eValue.t
        val obj = h.lookup(eValue.id).get
        val i = obj.t
        val mbody = info.mbody(m, i, j).get
        val j2 = mbody._1
        
        val isField = info.isField(i, j2, m)
        if (args.length == 0 && isField.isDefined) { //Getters
          val resId = obj.fields(isField.get._1)
          return Config(h, Value(isField.get._2, resId))
        } else { //Normal invocations
          val paramsMap: List[(String, Value)] = mbody._2.zip(args).map(p => {
            val argValueId = p._2 match { case Value(_, vid) => vid }
            (p._1.name, Value(p._1.paramType, argValueId))
          }) :+ ("this", Value(mbody._1, eValue.id))
          val returnExpr = AnnoExpr(mbody._3._1, mbody._3._2.get)
          Config(h, paramsMap.foldRight[Expr](returnExpr)((p1: (String, Value), p2: Expr) => p2.subst(p1._1, p1._2)))
        }
      }
      case InvkStatic(t, m, args) => {
        for (index <- 0 to args.length - 1) {
          if (!args(index).isValue)
            return eval1(info, Config(h, args(index))).update(x => InvkStatic(t, m, args.updated(index, x)))
        }
        val newObj = Obj(t, args.map(arg => arg match {case Value(_, id) => id}))
        val newHwithId = h.addObj(newObj)
        Config(newHwithId._1, Value(t, newHwithId._2)) // I.m(vs)
      }
      case AnnoExpr(i, Value(t, id)) => Config(h, Value(i, id))
      case AnnoExpr(i, e) => eval1(info, Config(h, e)).update(AnnoExpr(i, _))
      case InvkSetter(Value(i0, o0), fieldName, Value(i1, o1)) => {
        val obj = h.lookup(o0).get
        val field = info.isField(obj.t, i0, fieldName).get
        val newH = h.update(o0, field._1, o1)
        Config(newH, InvkStatic("Void", "of", List()))
      }
      case InvkSetter(e : Value, fieldName, para) => eval1(info, Config(h, para)).update(InvkSetter(e, fieldName, _))
      case InvkSetter(e, fieldName, para) => eval1(info, Config(h, e)).update(InvkSetter(_, fieldName, para))
      case LetExpr(t, x, Value(_, id), e2) => Config(h, e2.subst(x, Value(t, id)))
      case LetExpr(t, x, e1, e2) => eval1(info, Config(h, e1)).update(LetExpr(t, x, _, e2))
      case v : Value => throw Done(config)
    }
  }
  
  def eval(info: Info, config: Config): (String, String) = {
    try { println("==> " + config.toString()); val c = eval1(info, config); eval(info, c) } catch {
      case Done(Config(h, Value(t, id))) => ("type = " + t, "obj = " + h.lookup(id).get.pretty(h))
      case Done(_)                             => throw Buggy
      case e : Throwable                       => throw e
    }
  }
  
}