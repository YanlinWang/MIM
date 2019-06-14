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
        assert(eValue.caseID == 3, Buggy)
        val j = eValue.t
        val obj = h.lookup(eValue.id).get
        val i = obj.t
        val mbody = info.mbody(m, i, j).get
        val j2 = mbody._1
        
        val isField = info.isField(i, j2, m)
        if (args.length == 0 && isField.isDefined) { //Getters
          val fieldType = isField.get._2
          return Config(h, obj.fields(isField.get._1).substType(fieldType))
        } else { //Normal invocations
          val paramsMap: List[(String, Value)] = mbody._2.zip(args).map(p => {
            (p._1.name, p._2 match { case v: Value => v })
          }) :+ ("this", new Value(mbody._1, eValue.id))
          val returnExpr = AnnoExpr(mbody._3._1, mbody._3._2.get)
          Config(h, paramsMap.foldRight[Expr](returnExpr)((p1: (String, Value), p2: Expr) => p2.subst(p1._1, p1._2)))
        }
      }
      case InvkStatic(t, args) => {
        for (index <- 0 to args.length - 1) {
          if (!args(index).isValue)
            return eval1(info, Config(h, args(index))).update(x => InvkStatic(t, args.updated(index, x)))
        }
        val newObj = Obj(t, args.map(arg => arg match {case v: Value => v}))
        val newHwithId = h.addObj(newObj)
        Config(newHwithId._1, new Value(t, newHwithId._2)) // new I(vs)
      }
      case AnnoExpr(i, v@Value(caseID, lit, str, t, id)) => Config(h, v.substType(i))
      case AnnoExpr(i, e) => eval1(info, Config(h, e)).update(AnnoExpr(i, _))
      case InvkSetter(Value(3, _, _, i, o), fieldName, newV: Value) => {
        val obj = h.lookup(o).get
        val field = info.isField(obj.t, i, fieldName).get
        val newH = h.update(o, field._1, newV)
        Config(newH, InvkStatic("Void", List()))
      }
      case InvkSetter(e: Value, fieldName, para) => eval1(info, Config(h, para)).update(InvkSetter(e, fieldName, _))
      case InvkSetter(e, fieldName, para) => eval1(info, Config(h, e)).update(InvkSetter(_, fieldName, para))
      case LetExpr(t, x, v: Value, e2) => Config(h, e2.subst(x, v.substType(t)))
      case LetExpr(t, x, e1, e2) => eval1(info, Config(h, e1)).update(LetExpr(t, x, _, e2))
      case v : Value => throw Done(config)
    }
  }
  
  def eval(info: Info, config: Config): (String, String) = {
    try { println("==> " + config.toString()); val c = eval1(info, config); eval(info, c) } catch {
      case Done(Config(h, Value(caseID, lit, str, t, id))) => caseID match {
        case 1 => ("type = Int", "res = " + lit.toString)
        case 2 => ("type = String", "res = \"" + str + "\"")
        case 3 => ("type = " + t, "res = " + h.lookup(id).get.pretty(h))
      }
      case Done(_)                             => throw Buggy
      case e : Throwable                       => throw e
    }
  }
  
}