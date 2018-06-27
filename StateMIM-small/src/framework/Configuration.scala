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
  
  case class BS(varTable: List[(String, Value)]) {
    def addLet(x: String, v: Value) = BS((x, v) +: varTable)
    def remLet() = BS(varTable.tail)
    override def toString() = if (varTable.isEmpty) ""
      else varTable.map(p => p._1 + " = " + p._2).reduce((a, b) => a + "; " + b)
  }
  
  case class VS(scopes: List[BS]) {
    def addScope(scope: BS) = VS(scope +: scopes)
    def addLet(x: String, v: Value) = VS(scopes.head.addLet(x, v) +: scopes.tail)
    def remScope() = VS(scopes.tail)
    def remLet() = VS(scopes.head.remLet() +: scopes.tail)
    def getVar(x: String) = scopes.head.varTable.find(p => p._1 == x).get._2
    override def toString() = if (scopes.isEmpty) ""
      else scopes.map(p => "[" + p.toString + "]").reduce((a, b) => a + "\n\t " + b)
  }
  
  case class Config(h: H, vs: VS, e: Expr) {
    def update(f: Expr => Expr) = Config(h, vs, f(e))
    override def toString() = "H = " + h + "\n    VS = " + vs + "\n    Expr = " + e
  }
  
}