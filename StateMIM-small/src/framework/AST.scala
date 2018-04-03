package framework
import Configuration._

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
          if (error) None else Some(Info(table, typeMap, methodMap, is.map(x => x.name -> x).toMap))
        }
      }
    }
    def eval(): (String, String) = {
      val init = Config(H(Map(), 1), VS(List(BS(Map()))), e, FS(List()))
      Semantics.eval(collectInfo.get, init)
    }
  }
  
  case class TypeDef(name: String, sups: List[String], methods: List[MethDef], constr: Option[Constructor]) {}
  
  case class Field(fieldType: String, path: String, name: String) {}
  
  case class Parameter(paramType: String, name: String)
  
  case class Constructor(returnType: String, name: String, paras: List[Field]) {}
  
  case class MethDef(returnType: String, name: String, paras: List[Parameter], update: String, returnExpr: Option[Expr]) {}
  
  abstract class Expr {
    def isValue: Boolean = false
  }
  
  case class Var(x: String) extends Expr {}
  
  case class Invk(e: Expr, m: String, args: List[Expr]) extends Expr {}
  
  case class InvkStatic(t: String, m: String, args: List[Expr]) extends Expr {}
  
  case class AnnoExpr(i: String, e: Expr) extends Expr {}
  
  case class InvkSetter(e: Expr, fieldName: String, para: Expr) extends Expr {}
 
  case class LetExpr(t: String, x: String, e1: Expr, e2: Expr) extends Expr {}
  
  case class Value(t: String, id: OId) extends Expr {
    override def isValue = true
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
  
  case class ReturnExpr() extends OpenExpr {
    override def fill(v: Value) = throw Error.Buggy
  }
  
}