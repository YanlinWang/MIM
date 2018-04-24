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
      val init = Config(H(Map(), 1), VS(List(BS(List()))), e, FS(List()))
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
  
  case class Constructor(returnType: String, name: String, paras: List[Field]) {
    def constructorCheck(info: Info, thisType: String): Unit = {
      assert(thisType == returnType, Message("Constructor of " + thisType + " has wrong type."))
      for (para <- paras) {
        assert(info.table.contains(para.fieldType), TypeNotFound(para.fieldType))
        assert(info.table.contains(para.path), TypeNotFound(para.path))
      }
      assert(info.canInstantiate(this), Message("Invalid constructor of " + thisType + "."))
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
  }
  
  case class Var(x: String) extends Expr {
    def checkType(info: Info, env: Map[String, String]) = env.get(x) match {
      case Some(t) => t
      case None    => throw VarNotInScope(x)
    }
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
  }
  
  case class InvkStatic(t: String, m: String, args: List[Expr]) extends Expr {
    def checkType(info: Info, env: Map[String, String]) = { // TODO. Incorrect.
      assert(info.table.contains(t), TypeNotFound(t))
      val mtype = info.mtype(m, t)
      assert(mtype.isDefined, Message("method " + m + " is undefine."))
      val argsT = args.map(arg => arg.checkType(info, env))
      assert(mtype.get._1.size == argsT.size, Message("#Arguments unexpected."))
      assert(mtype.get._1.zip(argsT).forall(p => info.subType(p._2, p._1)), Message("Argument types unexpected."))
      mtype.get._2
    }
  }
  
  case class AnnoExpr(i: String, e: Expr) extends Expr {
    def checkType(info: Info, env: Map[String, String]) = {
      assert(info.table.contains(i), TypeNotFound(i))
      val eT = e.checkType(info, env)
      assert(info.subType(eT, i), Message(eT + " <: " + i + " does not hold."))
      i
    }
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
      eT //TODO: should return static type or?
    }
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
  }
  
  case class Value(t: String, id: OId) extends Expr {
    override def isValue = true
    def checkType(info: Info, env: Map[String, String]) = throw Buggy
  }
  
  abstract class OpenExpr {
    def fill(v: Value): Expr = throw Error.TODO
  }
  
  case class Invk_E(m: String, args: List[Expr]) extends OpenExpr {
    override def fill(v: Value) = Invk(v, m, args)
  }
  
  case class Invk_Arg(e: Expr, m: String, args: List[Expr], index: Int) extends OpenExpr {
    override def fill(v: Value) = Invk(e, m, args.updated(index, v))
  }
  
  case class InvkStatic_Arg(t: String, m: String, args: List[Expr], index: Int) extends OpenExpr {
    override def fill(v: Value) = InvkStatic(t, m, args.updated(index, v))
  }
  
  case class AnnoExpr_E(i: String) extends OpenExpr {
    override def fill(v: Value) = AnnoExpr(i, v)
  }
  
  case class InvkSetter_E(fieldName: String, para: Expr) extends OpenExpr {
    override def fill(v: Value) = InvkSetter(v, fieldName, para)
  }
  
  case class InvkSetter_Arg(e: Expr, fieldName: String) extends OpenExpr {
    override def fill(v: Value) = InvkSetter(e, fieldName, v)
  }
  
  case class LetExpr_E1(t: String, x: String, e2: Expr) extends OpenExpr {
    override def fill(v: Value) = LetExpr(t, x, v, e2)
  }
  
  case class ReturnExprForLet() extends OpenExpr {
    override def fill(v: Value) = throw Error.Buggy
  }
  
  case class ReturnExprForInvk() extends OpenExpr {
    override def fill(v: Value) = throw Error.Buggy
  }
  
}