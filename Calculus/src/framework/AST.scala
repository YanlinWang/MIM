package framework

object AST {
  case class Program(is: List[TypeDef], e: Expr) {
    def programCheck(info: Info): String = {
      var checked: Set[String] = Set()
      for (t <- info.table) {
        if (!checked(t)) {
          assert(info.typeMap.contains(t))
          for (t0 <- info.typeMap(t)) {
            if (!checked(t0)) {
              assert(info.typeMap.contains(t0))
              assert(info.typeDefMap(t0).interfaceCheck(info))
              checked += t0
            }
          }
          assert(info.typeDefMap(t).interfaceCheck(info))
          checked += t
        }
      }
      e.checkType(info, Map())
    }
  }
  
  case class TypeDef(name: String, sups: List[String], methods: List[MethDef]) {
    def interfaceCheck(info: Info): Boolean = {
      assert(sups.forall(x => info.table.contains(x)))
      assert(methods.forall(x => x.methodCheck(info, name)))
      val collectMethods = info.collectMethods(name)
      val cond1 = !(collectMethods.exists(m => info.table.exists(j => info.subType(name, j) &&
          info.mtype(m, j).isDefined && info.mbody(m, name, j).isEmpty)))
      val cond2 = !(collectMethods.exists(m => info.table.exists(j => info.subType(name, j) &&
          info.dispatch(name, m, name).isDefined && info.dispatch(j, m, j).isDefined &&
              !info.canOverride(m, name, j))))
      assert(cond1, "Type-checker: condition 2 unsatisfied for type " + name + " on interface check.")
      assert(cond2, "Type-checker: condition 3 unsatisfied for type " + name + " on interface check.")
      true
    }
  }
  
  case class MethDef(returnType: String, name: String, paras: List[(String, String)], update: String, returnExpr: Option[Expr]) {
    def methodCheck(info: Info, thisType: String) = {
      assert(info.table.contains(returnType))
      assert(info.table.contains(update))
      assert(paras.map(p => p._1).forall(x => info.table.contains(x)))
      assert(info.subType(thisType, update))
      if (returnExpr.isDefined) {
        val env: Map[String, String] = paras.map(p => p._2 -> p._1).toMap + ("this" -> thisType)
        val mtype = info.mtype(name, update)
        assert(mtype.isDefined && mtype.get._1.size == paras.size)
        assert(mtype.get._1.zip(paras).forall(p => p._1 == p._2._1))
        val checkType = returnExpr.get.checkType(info, env)
        assert(info.subType(checkType, returnType))
      }
      val mostSpecific = info.mostSpecific(name, thisType, update)
      assert(mostSpecific.size == 1 && mostSpecific.head == update)
      true
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
    def checkType(info: Info, env: Map[String, String]) = {
      assert(env.contains(n))
      env(n)
    }
    def subst(map: Map[String, Expr]) = if (map.contains(n)) map.get(n).get.copy else Var(n)
  }
  
  case class New(o: String) extends Expr {
    def copy = New(o)
    def checkType(info: Info, env: Map[String, String]) = {
      assert(info.table.contains(o))
      assert(info.canInstantiate(o))
      o
    }
    def subst(map: Map[String, Expr]) = New(o)
  }
  
  case class Invk(e: Expr, m: String, args: List[Expr]) extends Expr {
    def copy = Invk(e.copy, m, args.map(x => x.copy))
    def checkType(info: Info, env: Map[String, String]) = {
      val eT = e.checkType(info, env)
      val mtype = info.mtype(m, eT)
      assert(mtype.isDefined)
      val argsT = args.map(arg => arg.checkType(info, env))
      assert(mtype.get._1.size == argsT.size)
      assert(mtype.get._1.zip(argsT).forall(p => info.subType(p._2, p._1)))
      mtype.get._2
    }
    def subst(map: Map[String, Expr]) = Invk(e.subst(map), m, args.map(x => x.subst(map)))
  }
  
  case class AnnoExpr(i: String, e: Expr) extends Expr {
    def copy = AnnoExpr(i, e.copy)
    def checkType(info: Info, env: Map[String, String]) = {
      assert(info.table.contains(i))
      val eT = e.checkType(info, env)
      assert(info.subType(eT, i))
      i
    }
    def subst(map: Map[String, Expr]) = AnnoExpr(i, e.subst(map))
  }
}