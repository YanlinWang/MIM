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
      case Var(x) => Config(h, vs, vs.getVar(x).get, fs)
      case Invk(e, m, args) => {
        if (!e.isValue) return Config(h, vs, e, fs.addFrame(Invk_E(m, args)))
        for (index <- 0 to args.length - 1) {
          if (!args(index).isValue)
            return Config(h, vs, args(index), fs.addFrame(Invk_Arg(e, m, args, index)))
        }
        throw TODO // v.m(vs)
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
      case InvkSetter(e : Value, fieldName, para : Value) => throw TODO // v.set_f(v)
      case InvkSetter(e : Value, fieldName, para) => Config(h, vs, para, fs.addFrame(InvkSetter_Arg(e, fieldName)))
      case InvkSetter(e, fieldName, para) => Config(h, vs, e, fs.addFrame(InvkSetter_E(fieldName, para)))
      case LetExpr(t, x, Value(_, id), e2) => Config(h, vs.addMap(Map(x -> Value(t, id))), e2, fs.addFrame(ReturnExpr()))
      case LetExpr(t, x, e1, e2) => Config(h, vs, e1, fs.addFrame(LetExpr_E1(t, x, e2)))
      case v : Value => {
        if (fs.frames.isEmpty) throw Done(config)
        fs.frames.head match {
          case Left(_)  => throw Buggy
          case Right(ReturnExpr()) => Config(h, vs.remMap(), v, FS(fs.frames.tail))
          case Right(e) => Config(h, vs, e.fill(v), FS(fs.frames.tail))
        }
      }
    }
  }
  
  def eval(info: Info, config: Config): (String, String) = {
    try { val c = eval1(info, config); eval(info, c) } catch {
      case Done(Config(h, _, Value(t, id), _)) => ("type = " + t, "obj = " + h.lookup(id).get.pretty(h))
      case Done(_)                             => throw Buggy
      case e : Throwable                       => throw e
    }
  }
  
}