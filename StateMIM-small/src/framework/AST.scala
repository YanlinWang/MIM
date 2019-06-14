package framework
import Configuration._
import Error._

object AST {
  
  case class Program(is: List[TypeDef], e: Expr) {
    val collectInfo: Option[Info] = {
      var error: Boolean = false
      val table = is.map(x => x.name)
      error = table.distinct.size != table.size
      if (error) None else {
        val typeMap = is.map(x => {
          if (x.sups.distinct.size != x.sups.size) error = true
          if (x.sups.exists(y => !table.contains(y))) error = true
          x.name -> x.sups}).toMap
        if (error) None else {
          val methodMap = is.map(x => {
            val methods = x.methods.map(y => (y.name, y.update))
            if (methods.distinct.size != methods.size) error = true
            x.name -> x.methods}).toMap
          val constructorMap = is.map(i => i.name -> i.constr).toMap
          if (error) None else Some(Info(table, typeMap, methodMap, is.map(x => x.name -> x).toMap, constructorMap))
        }
      }
    }
    def eval(): (String, String) = {
      val init = Config(H(Map(), 1), e)
      Semantics.eval(collectInfo.get, init)
    }
    
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
  
  case class TypeDef(name: String, sups: List[String], methods: List[MethDef], constr: Option[Constructor]) {
    def interfaceCheck(info: Info): Unit = {
      for (x <- sups) assert(info.table.contains(x), TypeNotFound(x))
      for (x <- methods) try x.methodCheck(info, name) catch {case error: Throwable => nextError(error, MethError(x))}
      if (constr.isDefined) constr.get.constructorCheck(info, name)
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
  
  case class Field(fieldType: String, path: String, name: String) {}
  
  case class Parameter(paramType: String, name: String)
  
  case class Constructor(paras: List[Field]) {
    def constructorCheck(info: Info, thisType: String): Unit = {
      for (para <- paras) {
        assert(info.table.contains(para.fieldType), TypeNotFound(para.fieldType))
        assert(info.table.contains(para.path), TypeNotFound(para.path))
      }
      assert(info.canInstantiate(thisType, this), Message("Invalid constructor of " + thisType + "."))
    }
  }
  
  case class MethDef(returnType: String, name: String, paras: List[Parameter], update: String, returnExpr: Option[Expr]) {
    def methodCheck(info: Info, thisType: String): Unit = {
      assert(info.table.contains(returnType), TypeNotFound(returnType))
      assert(info.table.contains(update), TypeNotFound(update))
      for (p <- paras) assert(info.table.contains(p.paramType), TypeNotFound(p.paramType))
      assert(info.subType(thisType, update), Message(thisType + " <: " + update + " does not hold."))
      if (returnExpr.isDefined) {
        val env: Map[String, String] = paras.map(p => p.name -> p.paramType).toMap + ("this" -> thisType)
        val mtype = info.mtype(name, update)
        assert(mtype.isDefined, Message("mbody(" + name + ", " + update + ", " + update + ") = error."))
        assert(mtype.get._1.size == paras.size, Message("#Parameters unexpected."))
        assert(mtype.get._1.zip(paras).forall(p => p._1 == p._2.paramType) , Message("Parameter types unexpected."))
        val checkType = returnExpr.get.checkType(info, env)
        assert(info.subType(checkType, returnType), Message("Return type unexpected."))
      }
      val mostSpecific = info.mostSpecific(name, thisType, update)
      assert(mostSpecific.size == 1 && mostSpecific.head == update,
          Message("findOrigin(" + name + ", " + thisType + ", " + update + ") = error."))
    }
  }
  
  abstract class Expr {
    def isValue: Boolean = false
    def checkType(info: Info, env: Map[String, String]): String
    def subst(x: String, v: Value): Expr // warning: trivial subst. not capture avoiding.
  }
  
  case class Var(x: String) extends Expr {
    def checkType(info: Info, env: Map[String, String]) = env.get(x) match {
      case Some(t) => t
      case None    => throw VarNotInScope(x)
    }
    def subst(s: String, v: Value) = if (s == x) v else Var(x)
  }
  
  case class Invk(e: Expr, m: String, args: List[Expr]) extends Expr {
    def checkType(info: Info, env: Map[String, String]) = {
      val eT = e.checkType(info, env)
      val mtype = info.mtype(m, eT)
      assert(mtype.isDefined, Message("mbody(" + m + ", " + eT + ", " + eT + ") = error."))
      val argsT = args.map(arg => arg.checkType(info, env))
      assert(mtype.get._1.size == argsT.size, Message("#Arguments unexpected."))
      assert(mtype.get._1.zip(argsT).forall(p => info.subType(p._2, p._1)), Message("Argument types unexpected."))
      mtype.get._2
    }
    def subst(x: String, v: Value) = Invk(e.subst(x, v), m, args.map(_.subst(x, v)))
  }
  
  case class InvkStatic(t: String, args: List[Expr]) extends Expr {
    def checkType(info: Info, env: Map[String, String]) = {
      assert(info.table.contains(t), TypeNotFound(t))
      val constr = info.constructorMap(t)
      assert(constr.isDefined, Message("Constructor of " + t + " undefined."))
      assert(constr.get.paras.size == args.size, Message("Wrong arguments on invk of " + t + "'s constructor."))
      for (i <- 0 to args.size - 1) {
        val argType = args(i).checkType(info, env)
        assert(info.subType(argType, constr.get.paras(i).fieldType),
            Message("Wrong argument#" + (i + 1) + " type on invk of " + t + "'s constructor."))
      }
      t
    }
    def subst(x: String, v: Value) = InvkStatic(t, args.map(_.subst(x, v)))
  }
  
  case class AnnoExpr(i: String, e: Expr) extends Expr {
    def checkType(info: Info, env: Map[String, String]) = {
      assert(info.table.contains(i), TypeNotFound(i))
      val eT = e.checkType(info, env)
      assert(info.subType(eT, i), Message(eT + " <: " + i + " does not hold."))
      i
    }
    def subst(x: String, v: Value) = AnnoExpr(i, e.subst(x, v))
  }
  
  case class InvkSetter(e: Expr, fieldName: String, para: Expr) extends Expr {
    
    def checkType(info: Info, env: Map[String, String]) = {
      //e:eT
      val eT = e.checkType(info, env)
      //check fieldName is a field method from mbody(fieldName, eT, eT)
      val fieldType = info.isField(info, fieldName, eT)
      assert(fieldType.isDefined, Message(fieldName + " not a field in " + eT))
      // fieldName:efT, check paraT <: efT 
      assert(info.subType(para.checkType(info, env), fieldType.get), Message("para type <: " + fieldType.get + "does not hold"))
      "Void"
    }
    def subst(x: String, v: Value) = InvkSetter(e.subst(x, v), fieldName, para.subst(x, v))
  }
 
  case class LetExpr(t: String, x: String, e1: Expr, e2: Expr) extends Expr {
    def checkType(info: Info, env: Map[String, String]) = {
      // I in table
      assert(info.table.contains(t), TypeNotFound(t))
      //e1 type
      val e1T = e1.checkType(info, env)
      //check e1 type subtype t
      assert(info.subType(e1T, t), Message(e1T + " <: " + t + " does not hold."))
      //e2 type with x:t
      e2.checkType(info, env.+(x -> t))
    }
    def subst(s: String, v: Value) = LetExpr(t, x, e1.subst(s, v), e2.subst(s, v))
  }
  
  case class Value(caseID: Int, lit: Int, str: String, t: String, id: OId) extends Expr {
    def this(lit: Int)           = this(1, lit, "NULL", "NULL", Int.MinValue)
    def this(str: String)        = this(2, Int.MinValue, str, "NULL", Int.MinValue)
    def this(t: String, id: OId) = this(3, Int.MinValue, "NULL", t, id)
    def checkType(info: Info, env: Map[String, String]) = caseID match {
      case 1 => "Int"
      case 2 => "String"
      case 3 => throw Buggy
    }
    def subst(x: String, v: Value) = this
    override def isValue = true
    def toStringWithH(h: H) = caseID match {
      case 1 => lit.toString
      case 2 => "\"" + str + "\""
      case 3 => h.lookup(id).get.pretty(h)
    }
    def substType(t: String) = caseID match {
      case 1 if t == "Int"    => new Value(lit)
      case 2 if t == "String" => new Value(str)
      case 3                  => new Value(t, id)
    }
    override def toString = caseID match {
      case 1 => "Num(" + lit + ")"
      case 2 => "Str(\"" + str + "\")"
      case 3 => "Object(" + t + "," + id + ")"
    }
  }
  
}