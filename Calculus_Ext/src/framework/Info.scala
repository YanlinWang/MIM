package framework

import AST._

case class Info(table: List[String], typeMap: Map[String, List[String]],
    methodMap: Map[String, List[MethDef]], typeDefMap: Map[String, TypeDef]) {
  def ext(t1: String, t2: String): Boolean = {
    assert(table.contains(t1) && table.contains(t2), "Bug: ext.")
    typeMap.get(t1).contains(t2)
  }
  def subType(t1: String, t2: String): Boolean = {
    assert(table.contains(t1) && table.contains(t2), "Bug: subType.")
    if (t1 == t2) return true
    typeMap.get(t1).get.exists(x => subType(x, t2))
  }
  def dispatch(i: String, m: String, j: String): Option[MethDef] = {
    assert(table.contains(i) && table.contains(j), "Bug: dispatch.")
    methodMap.get(i).get.find(x => x.name == m && x.update.contains(j))
  }
  def prune(set: Set[String]): Set[String] = set.filter(i => !set.exists(j => j != i && subType(j, i)))
  def collectMethods(i: String): Set[String] = {
    assert(table.contains(i), "Bug: collectMethods.")
    val start = methodMap.get(i).get.map(x => x.name).toSet
    val op = (s: Set[String], x: String) => s ++ collectMethods(x)
    typeMap.get(i).get.foldLeft(start)(op)
  }
  def mbody(m: String, id: String, is: String): Option[(String, List[(String, String)], (String, Option[Expr]))] = {
    assert(table.contains(id) && table.contains(is), "Bug: mbody.")
    val set1 = mostSpecific(m, id, is)
    if (set1.size != 1) return None
    val i = set1.head
    val set2 = mostSpecificOverride(m, id, i)
    if (set2.size != 1) return None
    val j = set2.head
    val method = methodMap.get(j).get.find(x => x.name == m && x.update.contains(i))
    if (method.isEmpty) None else Some(j, method.get.paras, (method.get.returnType, method.get.returnExpr))
  }
  def mtype(m: String, i: String): Option[(List[String], String)] = {
    assert(table.contains(i), "Bug: mtype.")
    val body = mbody(m, i, i)
    if (body.isEmpty) None else Some(body.get._2.map(p => p._1), body.get._3._1)
  }
  def mostSpecific(m: String, i: String, j: String): Set[String] = {
    assert(table.contains(i) && table.contains(j), "Bug: mostSpecific.")
    val set = table.filter(k => subType(i, k) && (subType(k, j) || subType(j, k)) && dispatch(k, m, k).isDefined).toSet
    prune(set)
  }
  def mostSpecificOverride(m: String, i: String, j: String): Set[String] = {
    assert(table.contains(i) && table.contains(j), "Bug: mostSpecificOverride.")
    val set = table.filter(k => subType(k, j) && subType(i, k) && dispatch(k, m, j).isDefined).toSet
    prune(set)
  }
  def canOverride(m: String, i: String, j: String): Boolean = {
    assert(table.contains(i) && table.contains(j), "Bug: canOverride.")
    val meth = dispatch(i, m, i)
    val meth2 = dispatch(j, m, j)
    if (meth.isEmpty || meth2.isEmpty) return false
    meth.get.returnType == meth2.get.returnType && meth.get.paras.zip(meth2.get.paras).forall(p => p._1._1 == p._2._1)
  }
  def canInstantiate(i: String): Boolean = {
    assert(table.contains(i), "Bug: canInstantiate.")
    val methods = collectMethods(i)
    methods.forall(m => mostSpecific(m, i, i).forall(j => {
      val set = mostSpecificOverride(m, i, j)
      set.size == 1 && dispatch(set.head, m, j).exists(meth => meth.returnExpr.isDefined)
    }))
  }
}