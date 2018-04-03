package framework

import AST._

object Configuration {
  
  type OId = Int
  
  case class Obj(t: String, fields: List[OId]) {
    override def toString = t // TODO
    def pretty(h: H): String = {
      val printField: OId => String = x => h.lookup(x).get.pretty(h)
      val fieldsStr = if (fields.isEmpty) "" else fields.map(printField).reduce((a, b) => a + ", " + b)
      t + "[" + fieldsStr + "]"
    }
  }
  
  case class H(objTable: Map[OId, Obj], nextId: OId) {
    def lookup(id: OId): Option[Obj] = objTable.get(id)
    def addObj(obj: Obj) = (H(objTable + (nextId -> obj), nextId + 1), nextId)
    def update(key: OId, index: Int, newID: OId) : H = {
      val obj = objTable(key)
      val newObj = Obj(obj.t, obj.fields.updated(index, newID))
      H(objTable + (key -> newObj), nextId)
    }
  }
  
  case class BS(varTable: Map[String, Value]) {
//    def addVar(x: String, v: Value) = BS(varTable + {x -> v})
  }
  
  case class VS(scopes: List[BS]) {
    def addMap(m: Map[String, Value]) = VS(BS(m) +: scopes)
    def remMap() = VS(scopes.tail)
    def getVar(x: String) = scopes.head.varTable.get(x)
//    def addVar(x: String, v: Value) = VS(scopes.head.addVar(x, v) +: scopes.tail)
//    def addScope(thisValue: (String, Value), argsValue: List[(String, Value)]): VS = {
//      val bs = BS(Map(thisValue._1 -> thisValue._2) ++ argsValue)
//      VS(bs +: scopes)
//    }
  }
  
  case class FS(frames: List[Either[Expr, OpenExpr]]) {
    def addFrame(open: OpenExpr) = FS(Right(open) +: frames)
  }
  
  case class Config(h: H, vs: VS, expr: Expr, frameStack: FS)
  
}