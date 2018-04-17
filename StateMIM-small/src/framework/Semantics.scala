package framework

import Configuration._
import Error._
import AST._

object Semantics {
  
  def eval1(info: Info, config: Config): Config = {
    val h = config.h
    val vs = config.vs
    val fs = config.frameStack
    config.expr match {
      case Var(x) => Config(h, vs, vs.getVar(x), fs)
      case Invk(e, m, args) => {
        println("Evaluating: " + config.expr.toString())
        if (!e.isValue) return Config(h, vs, e, fs.addFrame(Invk_E(m, args)))
        for (index <- 0 to args.length - 1) {
          if (!args(index).isValue)
            return Config(h, vs, args(index), fs.addFrame(Invk_Arg(e, m, args, index)))
        }
        
        val eValue = e match { case v : Value => v } //cast e to value
        val j = eValue.t
        val obj = h.lookup(eValue.id).get
        val i = obj.t
        val mbody = info.mbody(m, i, j).get
        val j2 = mbody._1
        
        val isField = info.isField(i, j2, m)
        if (args.length == 0 && isField.isDefined) { //Getters
          println("Is getter")
          val resId = obj.fields(isField.get._1)
          Config(h, vs, Value(isField.get._2, resId), fs)
        } else { //Normal invocations
          println("Is not getter")
          val paramsMap: List[(String, Value)] = mbody._2.zip(args).map(p => {
            val argValueId = p._2 match { case Value(_, vid) => vid }
            (p._1.name, Value(p._1.paramType, argValueId))
          }) :+ ("this", eValue)
          val newScope = vs.addScope(BS(paramsMap))
          Config(h, newScope, AnnoExpr(mbody._3._1, mbody._3._2.get), fs.addFrame(ReturnExprForInvk()))
        }
      }
      case InvkStatic(t, m, args) => {
        for (index <- 0 to args.length - 1) {
          if (!args(index).isValue)
            return Config(h, vs, args(index), fs.addFrame(InvkStatic_Arg(t, m, args, index)))
        }
        val newObj = Obj(t, args.map(arg => arg match {case Value(_, id) => id}))
        val newHwithId = h.addObj(newObj)
        Config(newHwithId._1, vs, Value(t, newHwithId._2), fs) // I.m(vs)
      }
      case AnnoExpr(i, Value(t, id)) => Config(h, vs, Value(i, id), fs)
      case AnnoExpr(i, e) => Config(h, vs, e, fs.addFrame(AnnoExpr_E(i)))
      case InvkSetter(Value(i0, o0), fieldName, Value(i1, o1)) => {
        val obj = h.lookup(o0).get
        val field = info.isField(obj.t, i0, fieldName).get
        val newH = h.update(o0, field._1, o1)
        Config(newH, vs, Value(obj.t, o0), fs)
      }
      case InvkSetter(e : Value, fieldName, para) => Config(h, vs, para, fs.addFrame(InvkSetter_Arg(e, fieldName)))
      case InvkSetter(e, fieldName, para) => Config(h, vs, e, fs.addFrame(InvkSetter_E(fieldName, para)))
      case LetExpr(t, x, Value(_, id), e2) => Config(h, vs.addLet(x, Value(t, id)), e2, fs.addFrame(ReturnExprForLet()))
      case LetExpr(t, x, e1, e2) => Config(h, vs, e1, fs.addFrame(LetExpr_E1(t, x, e2)))
      case v : Value => {
        if (fs.frames.isEmpty) throw Done(config)
        fs.frames.head match {
          case Left(_)  => throw Buggy
          case Right(ReturnExprForLet()) => Config(h, vs.remLet(), v, FS(fs.frames.tail))
          case Right(ReturnExprForInvk()) => Config(h, vs.remScope(), v, FS(fs.frames.tail))
          case Right(e) => Config(h, vs, e.fill(v), FS(fs.frames.tail))
        }
      }
    }
  }
  
  def eval(info: Info, config: Config): (String, String) = {
    try { println("==> " + config.toString()); val c = eval1(info, config); eval(info, c) } catch {
      case Done(Config(h, _, Value(t, id), _)) => ("type = " + t, "obj = " + h.lookup(id).get.pretty(h))
      case Done(_)                             => throw Buggy
      case e : Throwable                       => throw e
    }
  }
  
}