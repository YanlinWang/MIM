package framework
import AST._

case class Info(table: List[String], typeMap: Map[String, List[String]],
      methodMap: Map[String, List[MethDef]], typeDefMap: Map[String, TypeDef]) {
  
  
  def isField(i: String, j: String, m: String): Option[(Int, String)] = {
    // see the of() method in class I.
    // see if argument J.m exists
    val typeDef = typeDefMap.get(i).get
    val constr = typeDef.constr
    if (constr.isEmpty) return None
    val paras = constr.get.paras
    for (x <- 0 to paras.length - 1) {
      val para = paras(x)
      if (para.path == j && para.name == m) return Some(x, para.fieldType)
    }
    None
  }
  
  def mbody(m: String, id: String, is: String): Option[(String, List[Parameter], (String, Option[Expr]))] = {
      if (!table.contains(id) || !table.contains(is)) throw new Exception("Error: mbody.")
      val set1 = mostSpecific(m, id, is)
      if (set1.size != 1) return None
      val i = set1.head
      val set2 = mostSpecificOverride(m, id, i)
      if (set2.size != 1) return None
      val j = set2.head
      val method = methodMap.get(j).get.find(x => x.name == m && x.update == i)
      //val error = throw new Exception()
      if (method.isEmpty) None else Some(j, method.get.paras, (method.get.returnType, method.get.returnExpr))
    }
  
  def mostSpecific(m: String, i: String, j: String): Set[String] = {
      if (!table.contains(i) || !table.contains(j)) throw new Exception("Error: mostSpecific.")
      val set = table.filter(k => subType(i, k) && (subType(k, j) || subType(j, k)) && dispatch(k, m, k).isDefined).toSet
      prune(set)
    }
    def mostSpecificOverride(m: String, i: String, j: String): Set[String] = {
      if (!table.contains(i) || !table.contains(j)) throw new Exception("Error: mostSpecificOverride.")
      val set = table.filter(k => subType(k, j) && subType(i, k) && dispatch(k, m, j).isDefined).toSet
      prune(set)
    }
    def subType(t1: String, t2: String): Boolean = {
      if (!table.contains(t1) || !table.contains(t2)) throw new Exception("Error: subType.")
      if (t1 == t2) return true
      typeMap.get(t1).get.exists(x => subType(x, t2))
    }
    def dispatch(i: String, m: String, j: String): Option[MethDef] = {
      if (!table.contains(i) || !table.contains(j)) throw new Exception("Error: dispatch.")
      methodMap.get(i).get.find(x => x.name == m && x.update == j)
    }
    def prune(set: Set[String]): Set[String] = set.filter(i => !set.exists(j => j != i && subType(j, i)))
}
      
      
object Aux {
  

      
  type OId = Int
  
  case class Obj(t: String, fields: List[OId]) {
    override def toString = t // TODO
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
    def addVar(x: String, v: Value) = BS(varTable + {x -> v})
  }
  
  case class VS(scopes: List[BS]) {
    def addVar(x: String, v: Value) = VS(scopes.head.addVar(x, v) +: scopes.tail)
    def getVar(x: String) = scopes.head.varTable.get(x)
    def addScope(thisValue: (String, Value), argsValue: List[(String, Value)]): VS = {
      val bs = BS(Map(thisValue._1 -> thisValue._2) ++ argsValue)
      VS(bs +: scopes)
    }
  }
  
}