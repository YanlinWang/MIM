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
  }
  
  case class TypeDef(name: String, sups: List[String], methods: List[MethDef], constr: Option[Constructor]) {}
  
  case class Field(fieldType: String, path: String, name: String) {}
  
  case class Parameter(paramType: String, name: String)
  
  case class Constructor(returnType: String, name: String, paras: List[Field]) {}
  
  case class MethDef(returnType: String, name: String, paras: List[Parameter], update: String, returnExpr: Option[Expr]) {}
  
  abstract class Expr {}
  
  case class Var(x: String) extends Expr {}
  
  case class Invk(e: Expr, m: String, args: List[Expr]) extends Expr {}
  
  case class InvkStatic(t: String, m: String, args: List[Expr]) extends Expr {}
  
  case class AnnoExpr(i: String, e: Expr) extends Expr {}
  
  case class InvkSetter(e: Expr, fieldName: String, para: Expr) extends Expr {}
 
  case class LetExpr(t: String, x: String, e1: Expr, e2: Expr) extends Expr {}
  
  case class Value(t: String, id: OId) extends Expr {}
  
  abstract class OpenExpr {}
  
  
  
}