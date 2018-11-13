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
  
  case class Config(h: H, e: Expr) {
    def update(f: Expr => Expr) = Config(h, f(e))
  }
  
}