package framework

object FHJ {
  /* Environment. */
  type Env = {val info: Info; val gamma: Map[String, String]}
  
  /* Abstract syntax tree. */
  /* Typing and semantic rules. */
  case class TypeDef(name: String, sups: List[String], methods: List[MethDef], constr: Option[MC]) {
    def interfaceCheck(env: Env): Boolean = {
      if (constr.isDefined) { if (!constr.get.mcCheck(env, name)) return false }
      if (sups.exists(x => !env.info.table.contains(x))) return false
      val env2: Env = new {val info = env.info; val gamma = env.gamma + ("this" -> name)}
      if (methods.exists(x => !x.methodCheck(env2))) return false
      val collectMethods = env.info.collectMethods(name)
      val cond1 = !(collectMethods.exists(m => env.info.table.exists(j => env.info.subType(name, j) &&
          env.info.mtype(m, j).isDefined && env.info.mbody(m, name, j).isEmpty)))
      val cond2 = !(collectMethods.exists(m => env.info.table.exists(j => env.info.subType(name, j) &&
          env.info.dispatch(name, m, name).isDefined && env.info.dispatch(j, m, j).isDefined &&
              !env.info.canOverride(m, name, j))))
      cond1 && cond2 && setterCheck()
    }
    def setterCheck(): Boolean = {
      methods.forall(method => {
        val s = method.parseSetter()
        if (s.isEmpty) return true //not a setter format
        val fieldName = s.get._1
        val paras = s.get._2
        val update = s.get._3
        val returnType = s.get._4
        if (paras.size != 1) return false
        val fieldType = paras.head._1;
        
//        val getter = findGetter(methods, fieldName
        throw new Exception("TODO")
      })
    }
     
    override def toString = {
      val supStr = if (sups.isEmpty) "" else " extends " + sups.reduce((a, b) => a + ", " + b)
      val mcStr = if(constr.isEmpty) "" else constr.get.toString() + "\n"
      val methodsStr = if (methods.size == 0) "" else  methods.map(x => x.toString).reduce((a, b) => a + "\n" + b) 
      "interface " + name + supStr + " " + "{\n" + mcStr + methodsStr + "\n}"
    }
  }
  
  
  case class Field(I: String, J: String, x: String) {}
  
  case class MC(returnType: String, name: String, paras: List[(String, String, String)]) {
    override def toString = {
      val parasStr = if (paras.isEmpty) "" else paras.map(p => p._1 + " " + p._2 + "." + p._3).reduce((a, b) => a + ", " + b)
      "  " + returnType + " " + name + "(" + parasStr + ");" 
    }
    def mcCheck(env: Env, i: String): Boolean = {
      if (!i.equals(returnType)) return false
      return validMC(i, paras, env)
    }
    def validMC(i: String, paras: List[(String, String, String)], env: Env): Boolean = {
      val fields = calFields(i, env)
      paras.size == fields.size && paras.forall(para => matchInList(para, fields))
    }
    def matchInList(para: (String, String, String), fields: List[Field]): Boolean = {
      fields.find(field => field.I == para._1 && field.J == para._2 && field.x == para._3).isDefined
    }
    def calFields(i: String, env:Env) : List[Field] = {
      val info = env.info;
      val methods: List[MethDef] = info.methodMap.getOrElse(i, throw new Exception("Type " + i + " undefined."))
      methods.foldLeft(List[Field]())((fields: List[Field], method: MethDef) => {
        if (isGetter(method, i, env)) {
          val field: Field = new Field(method.returnType, method.update, method.name)
          fields :+ field
        } else fields
      }) 
    }
    def isGetter(method: MethDef, i: String, env: Env): Boolean = {
      method.paras.isEmpty && method.returnExpr.isEmpty
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
    def parseSetter() : Option[(String, List[(String, String)], String, String)] = {
      if (!name.startsWith("set_")) return None
      val fieldName = name.substring(4)
      return Some(fieldName, paras, update, returnType)
    }
  }
  
  abstract class Expr {
    def copy: Expr
    def checkType(env: Env): Option[String]
    def subst(map: Map[String, Expr]): Expr
    //def eval(env: Env): Value
    def eval(h: H, sc: SC): (H, SC, Expr)
    override def toString: String
    
    def evaluate(h: H, sc: SC) : Value = {
      val c = eval(h, sc)
      c._3 match {
        case Value(is,id,vs) => Value(is,id,vs)
        case _ => c._3.evaluate(c._1, c._2)
      }
    }
  }
  
  case class SC(map: Map[String, (String, Value)])
  case class H(map: Map[String, (String, Map[String, Value])])
  
  
  
//  evaluate(h: H, sc: SC, e: Expr): (H, SC, Expr) = {
//    evaluate(e.eval(h, sc))
//  }
  
  
  case class Var(n: String) extends Expr {
    def copy = Var(n)
    def checkType(env: Env) = env.gamma.get(n)
    def subst(map: Map[String, Expr]) = if (map.contains(n)) map.get(n).get.copy else Var(n)
    //def eval(env: Env) = throw new Exception("Error: Var.eval.")
    def eval(h: H, sc: SC): (H, SC, Expr) = {
      val v: Value = sc.map(n)._2
      (h, sc, v)
    }
    override def toString = n
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
//    def eval(env: Env) = {
//      val eValue = e.eval(env)
//      val (tempI, tempJ) = (eValue.id, eValue.is)
//      val argsValue = args.map(x => x.eval(env))
//      val mbody = env.info.mbody(m, tempI, tempJ).get
//      if (mbody._3._2.isEmpty) throw new Exception("Error: Invk.eval.") 
//      val map: Map[String, Expr] = Map("this" -> AnnoExpr(mbody._1, New(tempI))) ++
//        mbody._2.zip(argsValue).map(p => p._1._2 -> AnnoExpr(p._1._1, New(p._2.id))).toMap
//      AnnoExpr(mbody._3._1, mbody._3._2.get.subst(map)).eval(env)
//    }
    def eval(h: H, sc: SC): (H, SC, Expr) = {
      throw new Exception("Invk eval unimplemented")
    }
    override def toString = {
      val argsStr = if (args.isEmpty) "" else args.map(x => x.toString).reduce((a, b) => a + ", " + b)
      e.toString + "." + m + "(" + argsStr + ")"
    }
  }
  
  //TODO: fill in definition of checkType and eval
  case class InvkStatic(t: String, m: String, args: List[Expr]) extends Expr {
    def copy = InvkStatic(t, m, args.map(x => x.copy))
    def checkType(env: Env): Option[String] = {
      return Some(t);
    }
    def subst(map: Map[String, Expr]) = InvkStatic(t, m, args.map(x => x.subst(map)))
//    def eval(env: Env) = {
//      throw new Exception("Error: InvkStatic.eval. unimplemented")
//    }
    def eval(h: H, sc: SC): (H, SC, Expr) = {
      val vs = args.map(x => x.evaluate(h, sc))
      val v = Value(t, t, vs)
      (h, sc, v)
//      throw new Exception("InvkStatic eval unimplemented")
    }
    override def toString = {
      val argsStr = if (args.isEmpty) "" else args.map(x => x.toString).reduce((a, b) => a + ", " + b)
      t.toString + "." + m + "(" + argsStr + ")"
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
//    def eval(env: Env) = e match {
//      case New(o) => Value(i, o)
//      case e0 => Value(i, e0.eval(env).id)
//    }
    def eval(h: H, sc: SC): (H, SC, Expr) = {
       val e2 =  e.evaluate(h, sc)
       (h, sc, Value(i, e2.id, e2.vs))
    }
    override def toString = "((" + i + ") " + e.toString + ")"
  }
  
  case class Value(is: String, id: String, vs: List[Value]) extends Expr {
    override def toString = {
      //if (paras.isEmpty) "" else paras.map(p => p._1 + " " + p._2).reduce((a, b) => a + ", " + b)
      val vsStr = if (vs.isEmpty) "" else vs.map(v => v.toString).reduce((a,b) => a + ", " + b)
      "<" + is + "> " + id + ".of(" + vsStr + ")"
    }
    
    def eval(h: H, sc: SC) = throw new Exception("Value.eval ERROR")

    def copy: Expr = throw new Exception("Value.copy ERROR")
    def checkType(env: Env): Option[String] = throw new Exception("Value.checkType ERROR")
    def subst(map: Map[String, Expr]): Expr = throw new Exception("Value.subst ERROR")
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
//      else Right(e.eval(new {val info = collectInfo.get; val gamma = Map[String, String]()}))
      else {
        val newH: H = new H(Map[String, (String, Map[String, Value])]())
        val newSC: SC = new SC(Map[String, (String, Value)]())
        val expr: Expr = e.eval(newH, newSC)._3
        expr match {
          case Value(a, b, c) => Right(Value(a,b,c))
          case _ => throw new Exception("reduction error")
        }
      }
    }
    override def toString = {
      val isStr = if (is.isEmpty) "" else is.map(x => x.toString).reduce((a, b) => a + "\n" + b) + "\n\n"
      isStr + e.toString
    }
  }
}