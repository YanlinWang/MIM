package framework

import AST._

object Configuration {
  
  type OId = Int
  
  case class Obj(t: String, fields: List[Value]) {
    override def toString = t
    def pretty(h: H): String = {
      val printField: Value => String = v => v.toStringWithH(h)
      val fieldsStr = if (fields.isEmpty) "" else fields.map(printField).reduce((a, b) => a + ", " + b)
      t + "[" + fieldsStr + "]"
    }
  }
  
  case class H(objTable: Map[OId, Obj], nextId: OId) {
    def lookup(id: OId): Option[Obj] = objTable.get(id)
    def addObj(obj: Obj) = (H(objTable + (nextId -> obj), nextId + 1), nextId)
    def update(key: OId, index: Int, newV: Value): H = {
      val obj = objTable(key)
      val newObj = Obj(obj.t, obj.fields.updated(index, newV))
      H(objTable + (key -> newObj), nextId)
    }
  }
  
  case class Config(h: H, e: Expr) {
    def update(f: Expr => Expr) = Config(h, f(e))
  }
  
}