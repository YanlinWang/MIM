package framework

import scala.util.parsing.combinator._
import scala.util.parsing.combinator.syntactical._

object FHJ extends StandardTokenParsers with PackratParsers {
  
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
    def overrideSet(i: String, j: String): Set[String] = {
      if (!table.contains(i) || !table.contains(j)) throw new Exception("Error: overrideSet.")
      methodMap.get(i).get.filter(x => x.update == j).map(x => x.name).toSet
    }
    def prune(set: Set[String]): Set[String] = set.filter(i => !set.exists(j => j != i && subType(j, i)))
    def collectMethods(i: String): Set[String] = {
      if (!table.contains(i)) throw new Exception("Error: collectMethods.")
      val start = methodMap.get(i).get.map(x => x.name).toSet
      val op = (s: Set[String], x: String) => s ++ collectMethods(x)
      typeMap.get(i).get.foldLeft(start)(op)
    }
    def mbody(m: String, id: String, is: String): Option[(String, List[(String, String)], (String, Expr))] = {
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
      val set1 = table.filter(k => subType(k, j) && subType(i, k) && overrideSet(k, k).contains(m)).toSet
      if (!set1.isEmpty) return prune(set1)
      val set2 = table.filter(k => subType(j, k) && overrideSet(k, k).contains(m)).toSet
      prune(set2)
    }
    def mostSpecificOverride(m: String, i: String, j: String): Set[String] = {
      if (!table.contains(i) || !table.contains(j)) throw new Exception("Error: mostSpecificOverride.")
      val set = table.filter(k => subType(k, j) && subType(i, k) && overrideSet(k, j).contains(m)).toSet
      prune(set)
    }
  }
  
  type Env = {val info: Info; val gamma: Map[String, String]}
  
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
    def run: Option[Value] = {
      if (programCheck.isLeft) None
      else Some(e.eval(new {val info = collectInfo.get; val gamma = Map[String, String]()}))
    }
    override def toString = {
      val isStr = if (is.isEmpty) "" else is.map(x => x.toString).reduce((a, b) => a + "\n" + b) + "\n\n"
      isStr + e.toString
    }
  }
  
  case class TypeDef(name: String, sups: List[String], methods: List[MethDef]) {
    def interfaceCheck(env: Env): Boolean = {
      if (sups.exists(x => !env.info.table.contains(x))) return false
      val env2: Env = new {val info = env.info; val gamma = env.gamma + ("this" -> name)}
      if (methods.exists(x => !x.methodCheck(env2))) return false
      val collectMethods = env.info.collectMethods(name)
      !(collectMethods.exists(m => env.info.table.exists(j => env.info.subType(name, j) &&
          env.info.mtype(m, j).isDefined && env.info.mbody(m, name, j).isEmpty)))
    }
    override def toString = {
      val supStr = if (sups.isEmpty) "" else " extends " + sups.reduce((a, b) => a + ", " + b)
      "interface " + name + supStr + " " +
        (if (methods.size == 0) "{}" else "{\n" + methods.map(x => x.toString).reduce((a, b) => a + "\n" + b) + "\n}")
    }
  }
  
  case class MethDef(returnType: String, name: String, paras: List[(String, String)], update: String, returnExpr: Expr) {
    def methodCheck(env: Env): Boolean = {
      if (!env.info.table.contains(returnType)) return false
      if (!env.info.table.contains(update)) return false
      if (paras.map(p => p._1).exists(x => !env.info.table.contains(x))) return false
      val thisType = env.gamma.get("this").get
      if (!env.info.subType(thisType, update)) return false
      val env2: Env = new {val info = env.info; val gamma = env.gamma ++ paras.map(p => p._2 -> p._1).toMap}
      val mtype = env.info.mtype(name, update)
      if (mtype.isEmpty || mtype.get._1.size != paras.size) return false
      if (mtype.get._1.zip(paras).exists(p => p._1 != p._2._1)) return false
      if (mtype.get._2 != returnType) return false
      val checkType = returnExpr.checkType(env2)
      if (checkType.isEmpty || !env.info.subType(checkType.get, returnType)) return false
      val mostSpecific = env.info.mostSpecific(name, thisType, update)
      mostSpecific.size == 1 && mostSpecific.head == update
    }
    override def toString = {
      val parasStr = if (paras.isEmpty) "" else paras.map(p => p._1 + " " + p._2).reduce((a, b) => a + ", " + b)
      "  " + returnType + " " + name + "(" + parasStr + ") override " + update + " {\n    return " +
        returnExpr.toString + ";\n  }"
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
    def checkType(env: Env) = if (env.info.table.contains(o)) Some(o) else None
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
      val map: Map[String, Expr] = Map("this" -> AnnoExpr(mbody._1, New(tempI))) ++
        mbody._2.zip(argsValue).map(p => p._1._2 -> AnnoExpr(p._1._1, New(p._2.id))).toMap
      AnnoExpr(mbody._3._1, mbody._3._2.subst(map)).eval(env)
    }
    override def toString = {
      val argsStr = if (args.isEmpty) "" else args.map(x => x.toString).reduce((a, b) => a + ", " + b)
      e.toString + "." + m + "(" + argsStr + ")"
    }
  }
  
  case class PathInvk(e: Expr, i: String, m: String, args: List[Expr]) extends Expr {
    def copy = PathInvk(e.copy, i, m, args.map(x => x.copy))
    def checkType(env: Env): Option[String] = {
      val eT = e.checkType(env)
      if (eT.isEmpty) return None
      if (!env.info.subType(eT.get, i)) return None
      val mtype = env.info.mtype(m, i)
      if (mtype.isEmpty) return None
      val argsT = args.map(arg => arg.checkType(env))
      if (mtype.get._1.size != argsT.size) return None
      val argsMismatch = mtype.get._1.zip(argsT).exists(p => p._2.isEmpty || !env.info.subType(p._2.get, p._1))
      if (argsMismatch) None else Some(mtype.get._2)
    }
    def subst(map: Map[String, Expr]) = PathInvk(e.subst(map), i, m, args.map(x => x.subst(map)))
    def eval(env: Env) = {
      val eValue = e.eval(env)
      val (tempI, tempK) = (eValue.id, i)
      val argsValue = args.map(x => x.eval(env))
      val mbody = env.info.mbody(m, tempI, tempK).get
      val map: Map[String, Expr] = Map("this" -> AnnoExpr(mbody._1, New(tempI))) ++
        mbody._2.zip(argsValue).map(p => p._1._2 -> AnnoExpr(p._1._1, New(p._2.id))).toMap
      AnnoExpr(mbody._3._1, mbody._3._2.subst(map)).eval(env)
    }
    override def toString = {
      val argsStr = if (args.isEmpty) "" else args.map(x => x.toString).reduce((a, b) => a + ", " + b)
      e.toString + "." + i + "::" + m + "(" + argsStr + ")"
    }
  }
  
  case class SuperInvk(i: String, m: String, args: List[Expr]) extends Expr {
    def copy = SuperInvk(i, m, args.map(x => x.copy))
    def checkType(env: Env): Option[String] = {
      val thisType = env.gamma.get("this")
      if (thisType.isEmpty) return None
      if (!env.info.ext(thisType.get, i)) return None
      val mtype = env.info.mtype(m, i)
      if (mtype.isEmpty) return None
      val argsT = args.map(arg => arg.checkType(env))
      if (mtype.get._1.size != argsT.size) return None
      val argsMismatch = mtype.get._1.zip(argsT).exists(p => p._2.isEmpty || !env.info.subType(p._2.get, p._1))
      if (argsMismatch) None else Some(mtype.get._2)
    }
    def subst(map: Map[String, Expr]) = SuperInvk(i, m, args.map(x => x.subst(map)))
    def eval(env: Env) = {
      val tempK = i
      val argsValue = args.map(x => x.eval(env))
      val mbody = env.info.mbody(m, tempK, tempK).get
      val map: Map[String, Expr] = Map("this" -> AnnoExpr(mbody._1, New(tempK))) ++
        mbody._2.zip(argsValue).map(p => p._1._2 -> AnnoExpr(p._1._1, New(p._2.id))).toMap
      AnnoExpr(mbody._3._1, mbody._3._2.subst(map)).eval(env)
    }
    override def toString = {
      val argsStr = if (args.isEmpty) "" else args.map(x => x.toString).reduce((a, b) => a + ", " + b)
      "super." + i + "::" + m + "(" + argsStr + ")"
    }
  }
  
  case class AnnoExpr(i: String, e: Expr) extends Expr {
    def copy = AnnoExpr(i, e.copy)
    def checkType(env: Env) = throw new Exception("Error: AnnoExpr.checkType.")
    def subst(map: Map[String, Expr]) = AnnoExpr(i, e.subst(map))
    def eval(env: Env) = e match {
      case New(o) => Value(i, o)
      case e0 => Value(i, e0.eval(env).id)
    }
    override def toString = "<" + i + "> " + e.toString
  }
  
  case class Value(is: String, id: String) {
    override def toString = "<" + is + "> new " + id + "()"
  }
  
  lexical.reserved += ("interface", "extends", "return", "new", "override", "super")
  lexical.delimiters += ("(", ")", "{", "}", ",", ".", "::", ";")
  
  val lcid = ident ^? { case id if id.charAt(0).isLower => id }
  val ucid = ident ^? { case id if id.charAt(0).isUpper => id }
  
  object Parser {
    val pP : PackratParser[Program] = rep(pTD) ~ pE ^^ { case l ~ e => new Program(l, e) }
    val pTD : PackratParser[TypeDef] =
      ("interface" ~> ucid) ~ ("extends" ~> repsep(ucid, ",") | success(List())) ~ ("{" ~> rep(pMD) <~ "}") ^^
        { case n ~ sups ~ ms => new TypeDef(n, sups, ms) }
    val pMD : PackratParser[MethDef] =
      ucid ~ lcid ~ ("(" ~> repsep(ucid ~ lcid ^^ { case t ~ n => (t, n) }, ",") <~ ")") ~
        ("override" ~> ucid) ~ ("{" ~> ("return" ~> pE <~ ";") <~ "}") ^^
          { case t ~ m ~ paras ~ update ~ e => new MethDef(t, m, paras, update, e) }
    val pE : PackratParser[Expr] =
      pE ~ ("." ~> lcid) ~ ("(" ~> repsep(pE, ",") <~ ")") ^^
        { case e ~ m ~ args => Invk(e, m, args) } |||
      pE ~ ("." ~> ucid) ~ ("::" ~> lcid) ~ ("(" ~> repsep(pE, ",") <~ ")") ^^
        { case e ~ sup ~ m ~ args => PathInvk(e, sup, m, args) } |||
      (("super" ~ ".") ~> ucid) ~ ("::" ~> lcid) ~ ("(" ~> repsep(pE, ",") <~ ")") ^^
        { case sup ~ m ~ args => SuperInvk(sup, m, args) } |||
      lcid ~ ("(" ~> repsep(pE, ",") <~ ")") ^^
        { case m ~ args => Invk(Var("this"), m, args) } |||
      ucid ~ ("::" ~> lcid) ~ ("(" ~> repsep(pE, ",") <~ ")") ^^
        { case sup ~ m ~ args => PathInvk(Var("this"), sup, m, args) } |||
      "new" ~> ucid <~ ("(" ~ ")") ^^ New ||| lcid ^^ Var ||| "(" ~> pE <~ ")" 
  }
  
  def parse(in: String): Option[Program] = phrase(Parser.pP)(new lexical.Scanner(in)) match {
      case t if t.successful => Some(t.get)
      case t                 => None
  }
  
  def run(p: String) = {
    val parsed = parse(p)
    if (parsed.isEmpty) println("Parse failed.")
    else {
      val pretty = parsed.get.toString
      println(pretty + "\n")
      val eval = parsed.get.run
      if (eval.isEmpty) println("Type-check failed.")
      else println("==> " + eval.get.toString)
    }
  }
  
  val program1 = "interface P {}" +
                 "interface FromA extends P {}" +
                 "interface FromB extends P {}" +
                 "interface FromC extends P {}" +
                 "interface A { P m() override A {return new FromA();}}" +
                 "interface B { P m() override B {return new FromB();}}" +
                 "interface C extends A, B {}" +
                 "new C().A::m()"
                 
  val program2 = "interface P {}" +
                 "interface FromA extends P {}" +
                 "interface FromB extends P {}" +
                 "interface FromC extends P {}" +
                 "interface A { P m() override A {return new FromA();}}" +
                 "interface B { P m() override B {return new FromB();}}" +
                 "interface C extends A, B { P m() override C {return new FromC();}}" +
                 "new C().A::m()"
                 
  val program3 = "interface P {}" +
                 "interface FromA extends P {}" +
                 "interface FromB extends P {}" +
                 "interface FromC extends P {}" +
                 "interface A { P m() override A {return new FromA();}}" +
                 "interface B { P m() override B {return new FromB();}}" +
                 "interface C extends A, B { P m() override A {return new FromC();}}" +
                 "new C().A::m()"
                 
  val program4 = "interface P {}" +
                 "interface FromA extends P {}" +
                 "interface FromB extends P {}" +
                 "interface FromC extends P {}" +
                 "interface A { P m() override A {return new FromA();}}" +
                 "interface B extends A { P m() override B {return new FromB();}}" +
                 "interface C extends A { P m() override C {return new FromC();}}" +
                 "interface D extends B, C {}" +
                 "new D().A::m()"
                 
  val program5 = "interface P {}" +
                 "interface FromA extends P {}" +
                 "interface FromB extends P {}" +
                 "interface FromC extends P {}" +
                 "interface A { P m() override A {return new FromA();}}" +
                 "interface B extends A { P m() override A {return new FromB();}}" +
                 "interface C extends A { P m() override A {return new FromC();}}" +
                 "interface D extends B, C {}" +
                 "new D().A::m()"
                 
  val program6 = "interface P {}" +
                 "interface FromDeck extends P {}" +
                 "interface FromLoggingDeck extends P {}" +
                 "interface FromDrawable extends P {}" +
                 "interface Deck { P draw() override Deck {return new FromDeck();}" +
                                 " P shuffleAndDraw() override Deck {return this.draw();}}" +
                 "interface LoggingDeck extends Deck { P draw() override LoggingDeck {return new FromLoggingDeck();}}" +
                 "interface Drawable { P draw() override Drawable {return new FromDrawable();}}" +
                 "interface DrawableLoggingDeck extends Drawable, LoggingDeck {}" +
                 "new DrawableLoggingDeck().shuffleAndDraw()"
  
  def main(args : Array[String]) = {
    println("----------------------------------")
    run(program1)
    println("----------------------------------")
    run(program2)
    println("----------------------------------")
    run(program3)
    println("----------------------------------")
    run(program4)
    println("----------------------------------")
    run(program5)
    println("----------------------------------")
    run(program6)
    println("----------------------------------")
  }
}