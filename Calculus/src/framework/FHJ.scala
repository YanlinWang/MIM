package framework

object FHJ {
  /* Environment. */
  type Env = {val info: Info; val gamma: Map[String, String]}
  
  /* Abstract syntax tree. */
  /* Typing and semantic rules. */
  case class TypeDef(name: String, sups: List[String], methods: List[MethDef]) {
    def interfaceCheck(env: Env): Boolean = {
      if (sups.exists(x => !env.info.table.contains(x))) return false
      val env2: Env = new {val info = env.info; val gamma = env.gamma + ("this" -> name)}
      if (methods.exists(x => !x.methodCheck(env2))) return false
      val collectMethods = env.info.collectMethods(name)
      val cond1 = !(collectMethods.exists(m => env.info.table.exists(j => env.info.subType(name, j) &&
          env.info.mtype(m, j).isDefined && env.info.mbody(m, name, j).isEmpty)))
      val cond2 = !(collectMethods.exists(m => env.info.table.exists(j => env.info.subType(name, j) &&
          env.info.dispatch(name, m, name).isDefined && env.info.dispatch(j, m, j).isDefined &&
              !env.info.canOverride(m, name, j))))
      cond1 && cond2
    }
    override def toString = {
      val supStr = if (sups.isEmpty) "" else " extends " + sups.reduce((a, b) => a + ", " + b)
      "interface " + name + supStr + " " +
        (if (methods.size == 0) "{}" else "{\n" + methods.map(x => x.toString).reduce((a, b) => a + "\n" + b) + "\n}")
    }
  }
  
  case class MethDef(returnType: String, name: String, paras: List[(String, String)], update: String, returnExpr: Option[Expr]) {
    def methodCheck(env: Env): Boolean = {
      if (!env.info.table.contains(returnType)) return false
      if (!env.info.table.contains(update)) return false
      if (paras.map(p => p._1).exists(x => !env.info.table.contains(x))) return false
      val thisType = env.gamma.get("this").get
      if (!env.info.subType(thisType, update)) return false
      if (returnExpr.isDefined) {
        val env2: Env = new {val info = env.info; val gamma = env.gamma ++ paras.map(p => p._2 -> p._1).toMap}
        val mtype = env.info.mtype(name, update)
        if (mtype.isEmpty || mtype.get._1.size != paras.size) return false
        if (mtype.get._1.zip(paras).exists(p => p._1 != p._2._1)) return false
        if (mtype.get._2 != returnType) return false
        val checkType = returnExpr.get.checkType(env2)
        if (checkType.isEmpty || !env.info.subType(checkType.get, returnType)) return false
      }
      val mostSpecific = env.info.mostSpecific(name, thisType, update)
      mostSpecific.size == 1 && mostSpecific.head == update
    }
    override def toString = {
      val parasStr = if (paras.isEmpty) "" else paras.map(p => p._1 + " " + p._2).reduce((a, b) => a + ", " + b)
      val body = if (returnExpr.isEmpty) ";" else " {\n    return " + returnExpr.get.toString + ";\n  }"
      "  " + returnType + " " + name + "(" + parasStr + ") override " + update + body
    }
  }
  
  abstract class Expr {
    def copy: Expr
    def checkType(env: Env): Option[String]
    def subst(map: Map[String, Expr]): Expr
    def eval(env: Env): Value
    override def toString: String
  }
  
  case class Var(n: String) extends Expr {
    def copy = Var(n)
    def checkType(env: Env) = env.gamma.get(n)
    def subst(map: Map[String, Expr]) = if (map.contains(n)) map.get(n).get.copy else Var(n)
    def eval(env: Env) = throw new Exception("Error: Var.eval.")
    override def toString = n
  }
  
  case class New(o: String) extends Expr {
    def copy = New(o)
    def checkType(env: Env) = if (env.info.table.contains(o) && env.info.canInstantiate(o)) Some(o) else None
    def subst(map: Map[String, Expr]) = New(o)
    def eval(env: Env) = new Value(o, o)
    override def toString = "new "+ o + "()"
  }
  
  case class Invk(e: Expr, m: String, args: List[Expr]) extends Expr {
    def copy = Invk(e.copy, m, args.map(x => x.copy))
    def checkType(env: Env): Option[String] = {
      val eT = e.checkType(env)
      if (eT.isEmpty) return None
      val mtype = env.info.mtype(m, eT.get)
      if (mtype.isEmpty) return None
      val argsT = args.map(arg => arg.checkType(env))
      if (mtype.get._1.size != argsT.size) return None
      val argsMismatch = mtype.get._1.zip(argsT).exists(p => p._2.isEmpty || !env.info.subType(p._2.get, p._1))
      if (argsMismatch) None else Some(mtype.get._2)
    }
    def subst(map: Map[String, Expr]) = Invk(e.subst(map), m, args.map(x => x.subst(map)))
    def eval(env: Env) = {
      val eValue = e.eval(env)
      val (tempI, tempJ) = (eValue.id, eValue.is)
      val argsValue = args.map(x => x.eval(env))
      val mbody = env.info.mbody(m, tempI, tempJ).get
      if (mbody._3._2.isEmpty) throw new Exception("Error: Invk.eval.") 
      val map: Map[String, Expr] = Map("this" -> AnnoExpr(mbody._1, New(tempI))) ++
        mbody._2.zip(argsValue).map(p => p._1._2 -> AnnoExpr(p._1._1, New(p._2.id))).toMap
      AnnoExpr(mbody._3._1, mbody._3._2.get.subst(map)).eval(env)
    }
    override def toString = {
      val argsStr = if (args.isEmpty) "" else args.map(x => x.toString).reduce((a, b) => a + ", " + b)
      e.toString + "." + m + "(" + argsStr + ")"
    }
  }
  
  case class AnnoExpr(i: String, e: Expr) extends Expr {
    def copy = AnnoExpr(i, e.copy)
    def checkType(env: Env) : Option[String] = {
      if (!env.info.table.contains(i)) return None
      val eT = e.checkType(env)
      if (eT.isDefined && env.info.subType(eT.get, i)) return Some(i) else return None
    }
    def subst(map: Map[String, Expr]) = AnnoExpr(i, e.subst(map))
    def eval(env: Env) = e match {
      case New(o) => Value(i, o)
      case e0 => Value(i, e0.eval(env).id)
    }
    override def toString = "((" + i + ") " + e.toString + ")"
  }
  
  case class Value(is: String, id: String) {
    override def toString = "<" + is + "> new " + id + "()"
  }
  
  /* Type-checker. */
  case class Info(table: List[String], typeMap: Map[String, List[String]],
      methodMap: Map[String, List[MethDef]], typeDefMap: Map[String, TypeDef]) {
    def ext(t1: String, t2: String): Boolean = {
      if (!table.contains(t1) || !table.contains(t2)) throw new Exception("Error: ext.")
      typeMap.get(t1).contains(t2)
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
      if (!table.contains(i)) throw new Exception("Error: collectMethods.")
      val start = methodMap.get(i).get.map(x => x.name).toSet
      val op = (s: Set[String], x: String) => s ++ collectMethods(x)
      typeMap.get(i).get.foldLeft(start)(op)
    }
    def mbody(m: String, id: String, is: String): Option[(String, List[(String, String)], (String, Option[Expr]))] = {
      if (!table.contains(id) || !table.contains(is)) throw new Exception("Error: mbody.")
      val set1 = mostSpecific(m, id, is)
      if (set1.size != 1) return None
      val i = set1.head
      val set2 = mostSpecificOverride(m, id, i)
      if (set2.size != 1) return None
      val j = set2.head
      val method = methodMap.get(j).get.find(x => x.name == m && x.update == i)
      if (method.isEmpty) None else Some(j, method.get.paras, (method.get.returnType, method.get.returnExpr))
    }
    def mtype(m: String, i: String): Option[(List[String], String)] = {
      if (!table.contains(i)) throw new Exception("Error: mtype.")
      val body = mbody(m, i, i)
      if (body.isEmpty) None else Some(body.get._2.map(p => p._1), body.get._3._1)
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
    def canOverride(m: String, i: String, j: String): Boolean = {
      if (!table.contains(i) || !table.contains(j)) throw new Exception("Error: canOverride.")
      val meth = dispatch(i, m, i)
      val meth2 = dispatch(j, m, j)
      if (meth.isEmpty || meth2.isEmpty) return false
      meth.get.returnType == meth2.get.returnType && meth.get.paras.zip(meth2.get.paras).forall(p => p._1._1 == p._2._1)
    }
    def canInstantiate(i: String): Boolean = {
      if (!table.contains(i)) throw new Exception("Error: canInstantiate.")
      val methods = collectMethods(i)
      methods.forall(m => mostSpecific(m, i, i).forall(j => {
        val set = mostSpecificOverride(m, i, j)
        set.size == 1 && dispatch(set.head, m, j).exists(meth => meth.returnExpr.isDefined)
      }))
    }
  }
  
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
    def interfaceCheck: ((Env, Boolean, Set[String]), String) => (Env, Boolean, Set[String]) = (tuple, i) => {
      val (env, res, checked) = tuple
      if (!res || checked.contains(i)) (env, res, checked) else {
        val newTuple = env.info.typeMap.get(i).get.foldLeft(tuple)(interfaceCheck)
        val checkSelf = env.info.typeDefMap.get(i).get.interfaceCheck(env)
        (env, newTuple._2 && checkSelf, newTuple._3 + i)
      }
    }
    def programCheck: Either[String, String] = {
      if (collectInfo.isEmpty) return Left("Incorrect Info")
      val env = new {val info = collectInfo.get; val gamma = Map[String, String]()}
      if (!env.info.table.foldLeft((env, true, Set[String]()))(interfaceCheck)._2) return Left("Interface check failed")
      val checkType = e.checkType(env)
      if (checkType.isEmpty) Left("Expression type-check failed") else Right(checkType.get)
    }
    def run: Either[String, Value] = {
      val program_check = programCheck
      if (program_check.isLeft) Left(program_check.left.get)
      else Right(e.eval(new {val info = collectInfo.get; val gamma = Map[String, String]()}))
    }
    override def toString = {
      val isStr = if (is.isEmpty) "" else is.map(x => x.toString).reduce((a, b) => a + "\n" + b) + "\n\n"
      isStr + e.toString
    }
  }
}