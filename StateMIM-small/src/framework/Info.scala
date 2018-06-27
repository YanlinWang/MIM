package framework
import AST._

case class Info(table: List[String], typeMap: Map[String, List[String]],
      methodMap: Map[String, List[MethDef]], typeDefMap: Map[String, TypeDef], 
      constructorMap: Map[String, Option[Constructor]]) {
  
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
  def mtype(m: String, i: String): Option[(List[String], String)] = {
    assert(table.contains(i), "Bug: mtype.")
    val body = mbody(m, i, i)
    if (body.isEmpty) None else Some(body.get._2.map(p => p.paramType), body.get._3._1)
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
  def collectMethods(i: String): Set[String] = {
    assert(table.contains(i), "Bug: collectMethods.")
    val start = methodMap.get(i).get.map(x => x.name).toSet
    val op = (s: Set[String], x: String) => s ++ collectMethods(x)
    typeMap.get(i).get.foldLeft(start)(op)
  }
  def canOverride(m: String, i: String, j: String): Boolean = {
    assert(table.contains(i) && table.contains(j), "Bug: canOverride.")
    val meth = dispatch(i, m, i)
    val meth2 = dispatch(j, m, j)
    if (meth.isEmpty || meth2.isEmpty) return false
    meth.get.returnType == meth2.get.returnType && meth.get.paras.zip(meth2.get.paras).forall(p => p._1.paramType == p._2.paramType)
  }
  def canInstantiate(c: Constructor): Boolean = {
    val i = c.returnType
    assert(table.contains(i), "Bug: canInstantiate.")
    val methods = collectFieldsAndOthers(i)
    c.paras.toSet.equals(methods._1) && methods._2.forall(m => mostSpecific(m, i, i).forall(j => {
      val set = mostSpecificOverride(m, i, j)
      set.size == 1 && dispatch(set.head, m, j).exists(meth => meth.returnExpr.isDefined)
    }))
  }
  def collectFieldsAndOthers(i: String): (Set[Field], Set[String]) = {
    val op = (p: (Set[Field], Set[String]), m: MethDef) => {
      if (m.paras.size == 0 && m.returnExpr.isEmpty) (p._1 + AST.Field(m.returnType, i, m.name), p._2)
      else (p._1, p._2 + m.name)
    }
    val res: (Set[Field], Set[String]) = methodMap(i).foldLeft((Set[Field](), Set[String]()))(op)
    val op2 = (p: (Set[Field], Set[String]), x: String) => {
      val q = collectFieldsAndOthers(x)
      (p._1 ++ q._1, p._2 ++ q._2)
    }
    typeMap(i).foldLeft(res)(op2)
  }
  def isField(info: Info, name: String, t: String) : Option[String] = {
    val mbody = info.mbody(name, t, t)
    if (mbody.isEmpty) return None
    if (mbody.get._2.size != 0) return None
    if (mbody.get._3._2.isDefined) return None
    Some(mbody.get._3._1) //return field type
  }
}
      
     