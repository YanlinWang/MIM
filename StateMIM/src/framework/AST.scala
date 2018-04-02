package framework
import Aux._

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
    def eval(): (String, Obj) = {
      val res = e.eval(collectInfo.get, H(Map(), 1), VS(List(BS(Map()))))
      val value = res._2
      (value.t, res._1.lookup(value.id).get)
    }
  }
  
  case class TypeDef(name: String, sups: List[String], methods: List[MethDef], constr: Option[Constructor]) {
 
  }
  
  
  case class Field(fieldType: String, path: String, name: String) {}
  
  case class Parameter(paramType: String, name: String)
  
  case class Constructor(returnType: String, name: String, paras: List[Field]) {
  }
  
  case class MethDef(returnType: String, name: String, paras: List[Parameter], update: String, returnExpr: Option[Expr]) {
    
  }
  
  abstract class Expr {
    def eval(info: Info, h: H, vs: VS): (H, Value)
  }
  
  case class Var(x: String) extends Expr {
    
    def eval(info: Info, h: H, vs: VS): (H, Value) = (h, vs.getVar(x).get)
   
  }
  
  case class Invk(e: Expr, m: String, args: List[Expr]) extends Expr {
    
    def eval(info: Info, h: H, vs: VS): (H, Value) = {
      val res = e.eval(info, h, vs)
      val h0 = res._1
      val value = res._2
      val j = value.t
      val obj = h0.lookup(value.id).get
      val i = obj.t
      val isField = info.isField(i, j, m)
      if (args.length == 0 && isField.isDefined) {
        val resId = obj.fields(isField.get._1)
        (h0, Value(isField.get._2, resId))
      } else {
        val argsFold: (H, List[Value]) = args.foldLeft((h0, List[Value]()))((old, arg) => {
          val newRes = arg.eval(info, old._1, vs)
          (newRes._1, old._2 :+ newRes._2)
        })
        val mbody = info.mbody(m, i, j).get
        val newScope = vs.addScope(("this", value), mbody._2.zip(argsFold._2).map(p => {
          val argP = p._1
          val argV = p._2
          (argP.name, Value(argP.paramType, argV.id))
        }))
        val invkCall = mbody._3._2.get.eval(info, argsFold._1, newScope)
        (invkCall._1, Value(mbody._3._1, invkCall._2.id))
      }
    }
    
  }
  
  case class InvkStatic(t: String, m: String, args: List[Expr]) extends Expr {

    def eval(info: Info, h: H, vs: VS): (H, Value) = {
      // check that m has been defined in t (ensured by type checker)
      val argsFold: (H, List[Value]) = args.foldLeft((h, List[Value]()))((old, arg) => {
        val res = arg.eval(info, old._1, vs)
        (res._1, old._2 :+ res._2)
      })
      // check that values match the types (ensured)
      val newObj = Obj(t, argsFold._2.map(x => x.id))
      val h2: (H, OId) = argsFold._1.addObj(newObj)
      (h2._1, Value(t, h2._2))
    }
  }
  
  case class AnnoExpr(i: String, e: Expr) extends Expr {
    
    def eval(info: Info, h: H, vs: VS): (H, Value) = {
      val eRes = e.eval(info, h, vs)
      val h0 = eRes._1
      val value = eRes._2 //(I)o
      (h0, Value(i, value.id))
    }
  }
  
  case class InvkSetter(e: Expr, fieldName: String, para: Expr) extends Expr {
    
    def eval(info: Info, h: H, vs: VS): (H, Value) = {
      val eRes = e.eval(info, h, vs)
      val h0 = eRes._1
      val value = eRes._2 //(I0)o0
      val I0 = value.t
      val o0 = value.id
      val obj_o0 = h0.lookup(value.id).get //K[o1...on]
      val K = obj_o0.t
      
      val paraRes = para.eval(info, h0, vs)
      val h1 = paraRes._1
      val value2 = paraRes._2 //(I1)o'
      
      val field = info.isField(K, I0, fieldName).get
      val h_new = h1.update(o0, field._1, value2.id)
      (h_new, Value(K, o0))
    }
  }
 
  case class LetExpr(t: String, x: String, e1: Expr, e2: Expr) extends Expr {
    
    def eval(info: Info, h: H, vs: VS): (H, Value) = {
      val e1Res: (H, Value) = e1.eval(info, h, vs)
      val e2Res: (H, Value) = e2.eval(info, e1Res._1, vs.addVar(x, e1Res._2))
      e2Res
    }
  }
  
  
  case class Value(t: String, id: OId) extends Expr {
    
    def eval(info: Info, h: H, vs: VS): (H, Value) = throw new Exception("todo")
  }
}