package framework

import CompilationError._

object AST {
  case class Program(is: List[TypeDef], e: Expr) {
    def programCheck(info: Info): String = {
      var checked: Set[String] = Set()
      for (t <- info.table) {
        if (!checked(t)) {
          assert(info.typeMap.contains(t), TypeNotFound(t))
          for (t0 <- info.typeMap(t)) {
            if (!checked(t0)) {
              assert(info.typeMap.contains(t0), TypeNotFound(t0))
              try info.typeDefMap(t0).interfaceCheck(info) catch {case error: Throwable => nextError(error, TypeError(t0))}
              checked += t0
            }
          }
          try info.typeDefMap(t).interfaceCheck(info) catch {case error: Throwable => nextError(error, TypeError(t))}
          checked += t
        }
      }
      e.checkType(info, Map())
    }
  }
  
  case class TypeDef(name: String, sups: List[String], methods: List[MethDef]) {
    def interfaceCheck(info: Info): Unit = {
      for (x <- sups) assert(info.table.contains(x), TypeNotFound(x))
      for (x <- methods) try x.methodCheck(info, name) catch {case error: Throwable => nextError(error, MethError(x))}
      val collectMethods = info.collectMethods(name)
      val cond1 = !(collectMethods.exists(m => info.table.exists(j => info.subType(name, j) &&
          info.mtype(m, j).isDefined && info.mbody(m, name, j).isEmpty)))
      val cond2 = !(collectMethods.exists(m => info.table.exists(j => info.subType(name, j) &&
          info.dispatch(name, m, name).isDefined && info.dispatch(j, m, j).isDefined &&
              !info.canOverride(m, name, j))))
      assert(cond1, Message("Condition 2 unsatisfied for interface " + name + "."))
      assert(cond2, Message("Condition 3 unsatisfied for interface " + name + "."))
    }
  }
  
  case class MethDef(returnType: String, name: String, paras: List[(String, String)], update: String, returnExpr: Option[Expr]) {
    def methodCheck(info: Info, thisType: String): Unit = {
      assert(info.table.contains(returnType), TypeNotFound(returnType))
      assert(info.table.contains(update), TypeNotFound(update))
      for (p <- paras) assert(info.table.contains(p._1), TypeNotFound(p._1))
      assert(info.subType(thisType, update), Message(thisType + " <: " + update + " does not hold."))
      if (returnExpr.isDefined) {
        val env: Map[String, String] = paras.map(p => p._2 -> p._1).toMap + ("this" -> thisType)
        val mtype = info.mtype(name, update)
        assert(mtype.isDefined, Message("mbody(" + name + ", " + update + ", " + update + ") = error."))
        assert(mtype.get._1.size == paras.size, Message("#Parameters unexpected."))
        assert(mtype.get._1.zip(paras).forall(p => p._1 == p._2._1) , Message("Parameter types unexpected."))
        val checkType = returnExpr.get.checkType(info, env)
        assert(info.subType(checkType, returnType), Message("Return type unexpected."))
      }
      val mostSpecific = info.mostSpecific(name, thisType, update)
      assert(mostSpecific.size == 1 && mostSpecific.head == update,
          Message("findOrigin(" + name + ", " + thisType + ", " + update + ") = error."))
    }
  }
  
  abstract class Expr {
    def copy: Expr
    def checkType(info: Info, env: Map[String, String]): String
    def subst(map: Map[String, Expr]): Expr
    override def toString: String
  }
  
  case class Var(n: String) extends Expr {
    def copy = Var(n)
    def checkType(info: Info, env: Map[String, String]) = env.get(n) match {
      case Some(t) => t
      case None    => throw VarNotInScope(n)
    }
    def subst(map: Map[String, Expr]) = if (map.contains(n)) map.get(n).get.copy else Var(n)
  }
  
  case class New(o: String) extends Expr {
    def copy = New(o)
    def checkType(info: Info, env: Map[String, String]) = {
      assert(info.table.contains(o), TypeNotFound(o))
      assert(info.canInstantiate(o), Message(o + " cannot be instantiated."))
      o
    }
    def subst(map: Map[String, Expr]) = New(o)
  }
  
  case class Invk(e: Expr, m: String, args: List[Expr]) extends Expr {
    def copy = Invk(e.copy, m, args.map(x => x.copy))
    def checkType(info: Info, env: Map[String, String]) = {
      val eT = e.checkType(info, env)
      val mtype = info.mtype(m, eT)
      assert(mtype.isDefined, Message("mbody(" + m + ", " + eT + ", " + eT + ") = error."))
      val argsT = args.map(arg => arg.checkType(info, env))
      assert(mtype.get._1.size == argsT.size, Message("#Arguments unexpected."))
      assert(mtype.get._1.zip(argsT).forall(p => info.subType(p._2, p._1)), Message("Argument types unexpected."))
      mtype.get._2
    }
    def subst(map: Map[String, Expr]) = Invk(e.subst(map), m, args.map(x => x.subst(map)))
  }
  
  case class AnnoExpr(i: String, e: Expr) extends Expr {
    def copy = AnnoExpr(i, e.copy)
    def checkType(info: Info, env: Map[String, String]) = {
      assert(info.table.contains(i), TypeNotFound(i))
      val eT = e.checkType(info, env)
      assert(info.subType(eT, i), Message(eT + " <: " + i + " does not hold."))
      i
    }
    def subst(map: Map[String, Expr]) = AnnoExpr(i, e.subst(map))
  }
}