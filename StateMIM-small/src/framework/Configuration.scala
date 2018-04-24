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
  }
  
  case class VS(scopes: List[BS]) {
    def addScope(scope: BS) = VS(scope +: scopes)
    def addLet(x: String, v: Value) = VS(scopes.head.addLet(x, v) +: scopes.tail)
    def remLet() = VS(scopes.head.remLet() +: scopes.tail)
    def remScope() = VS(scopes.tail)
    def getVar(x: String) = scopes.head.varTable.find(p => p._1 == x).get._2
//    def addVar(x: String, v: Value) = VS(scopes.head.addVar(x, v) +: scopes.tail)
//    def addScope(thisValue: (String, Value), argsValue: List[(String, Value)]): VS = {
//      val bs = BS(Map(thisValue._1 -> thisValue._2) ++ argsValue)
//      VS(bs +: scopes)
//    }
  }
  
  case class FS(frames: List[Either[Expr, OpenExpr]]) {
    def addFrame(open: OpenExpr) = FS(Right(open) +: frames)
  }
  
  case class Config(h: H, vs: VS, expr: Expr, frameStack: FS) {
    override def toString() = {
      val tab = "\n      "
      "Config(" + tab + "expr = " + expr.toString() + tab + "fs = " + frameStack.toString() +
        tab + "vs = " + vs.toString() + tab + "h = " + h.toString() + "\n    )"
    }
  }
  
}