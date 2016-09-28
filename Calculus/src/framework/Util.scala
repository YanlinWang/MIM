package framework

import scala.util.parsing.combinator._
import scala.util.parsing.combinator.syntactical._

object Util extends StandardTokenParsers with PackratParsers {
  
  def noDuplicate[E](l : List[E]) : Boolean = l.size == l.toSet.size
  
  class Info(
      ts : Set[String],
      dMap : Map[String, Set[String]],
      mMap : Map[String, Set[MethDef]],
      noOL : Boolean) {
    val traits = ts
    val directUpMap = dMap
    val methodMap = mMap
    val methodNameMap = mMap.mapValues{x => x.map{y => y.name}}
    val upMapOption : Option[Map[String, Set[String]]] = {
      def f : (String, Set[String]) => Option[Set[String]] = (x, s) => {
        val setX : Set[String] = directUpMap.getOrElse(x, Set())
        if (directUpMap.get(x).isEmpty || !(setX & s).isEmpty) None else {
          val set = setX.map{x => f(x, s + x).map{y => y + x}}
          if (!set.forall{x => x.isDefined}) None
          else Some(set.foldLeft(Set[String]())((x, y) => x ++ y.get))
        }
      }
      if (traits.isEmpty) Some(Map()) else {
        val res : Set[Option[Map[String, Set[String]]]] =
          traits.map{x => f(x, Set(x)).map{y => Map(x -> y)}}
        if (!res.forall{x => x.isDefined}) None
        else Some(res.foldLeft(Map[String, Set[String]]())((x, y) => x ++ y.get))
      }
    }
    val noCycle : Boolean = upMapOption.isDefined
    val noOverloading : Boolean = noOL
    val infoCheck : Boolean = noCycle && noOverloading
    val upMap : Map[String, Set[String]] = upMapOption.getOrElse(Map()) // on infoCheck
    val mBody : Map[String, Map[String, Set[String]]] = { // on infoCheck
      upMap.toSet[(String, Set[String])]
           .foldLeft(Map[String, Map[String, Set[String]]]())((accM, p) => accM + (p._1 -> {
        (p._2 + p._1).map{x => methodNameMap.get(x).get}
                     .foldLeft(Set[String]())((x, y) => x ++ y)
                     .foldLeft(Map[String, Set[String]]())((acc, m) => acc + (m -> {
          if (methodNameMap.get(p._1).get(m)) Set(p._1)
          else {
            val ups_m = p._2.filter{x => methodNameMap.get(x).get(m)}
            val f = (x : String) => upMap.get(x).get
            val g = (x : Set[String], y : Set[String]) => Set() ++ x ++ y
            val needed = if (ups_m.isEmpty) Set[String]() else ups_m -- ups_m.map(f).reduce(g)
            val error = needed.exists{x =>
                        needed.exists{y => x != y && !(upMap.get(x).get & upMap.get(y).get).isEmpty}}
            if (error) Set[String]() else needed
          }
        }))
      }))
    }
    def merge(env : Info) : Info = {
      new Info(Set() ++ traits ++ env.traits,
          Map() ++ directUpMap ++ env.directUpMap,
          Map() ++ methodMap ++ env.methodMap,
          noOverloading && env.noOverloading)
    }
    def subType(s1 : String, s2 : String) : Boolean = s1 == s2 || upMap.get(s1).get(s2)
  }
  
  type VarEnv = Map[String, String]
  class Env(i : Info, v : VarEnv) {
    val info : Info = i
    val vars : VarEnv = v
    def addVar(x : (Type, String)) : Env =
      new Env(info, vars + (x._2 -> x._1.toString))
    def addVars(xs : List[(Type, String)]) : Env =
      new Env(info, vars ++ xs.map{x => (x._2, x._1.toString)}.toMap)
  }
  
  class Program(l : List[TraitDef], e : Expr) {
    val expr = e
    override val toString = {
      val f = (x : String, y : String) => x + "\n" + y
      val x = if (l.isEmpty) "" else l.map{x => x.toString}.reduce(f) + "\n"
      x + e.toString
    }
    val env : Info = l.foldLeft(new Info(Set(), Map(), Map(), true))((x, y) => x.merge(y.env))
    val checkType : Env => Option[String] = e.checkType
    val typeChecked : Env => Option[String] = env => {
      val wellTyped = env.info.infoCheck && env.info.mBody.forall(p => p._2.forall(q => q._2.size > 0)) &&
                      l.forall{x => x.typeChecked(env)}
      if (!wellTyped) None else checkType(env)
    }
  }
  
  class TraitDef(n : String, sups : List[Type], ms : List[MethDef]) {
    override val toString = {
      val f = (x : String, y : String) => x + ", " + y
      val x = if (sups.isEmpty) "" else " extends " + sups.map{x => x.toString}.reduce(f)
      val g = (x : String, y : String) => x + "\n" + y
      val y = if (ms.isEmpty) "" else "\n" + ms.map{x => "  " + x.toString}.reduce(g) + "\n"
      "class " + n + x + " {" + y + "}"
    }
    val env : Info = new Info(Set(n), Map(n -> sups.map{x => x.toString}.toSet),
        Map(n -> ms.toSet), noDuplicate(ms.map{x => x.name}))
    val typeChecked : Env => Boolean = env => ms.forall{x => x.typeChecked(env)}
  }
  
  class MethDef(t : Type, n : String, paras : List[(Type, String)], e : Expr) {
    val returnType = t
    val name = n
    val parameters = paras
    val returnExpr = e
    override val toString = {
      val f = (x : String, y : String) => x + ", " + y 
      val x = if (paras.isEmpty) "" else paras.map{x => x._1.toString + " " + x._2}.reduce(f)
      t.toString + " " + n + "(" + x + ") { return " + e.toString + "; }"
    }
    val typeChecked : Env => Boolean = env => {
      noDuplicate(paras.map{x => x._2}) &&
      paras.forall{p => p._1.isBounded(env)} &&
      e.checkType(env.addVars(paras)).exists{x => env.info.subType(x, t.toString)}
    }
  }
  
  class Type(n : String) {
    override val toString = n
    val isBounded : Env => Boolean = e => e.info.traits(n)
  }
  
  class Expr {
    val checkType : Env => Option[String] = _ => None
  }
  
  class Var(n : String) extends Expr {
    override val toString = n
    override val checkType : Env => Option[String] = env => env.vars.get(n)
  }
  
  class Object(n : Type) extends Expr  {
    override val toString = "new " + n.toString + "()"
     override val checkType : Env => Option[String] = env =>
       if (env.info.traits(n.toString)) Some(n.toString) else None
  }
  
  class MethCall(e : Expr, m : String, args : List[Expr]) extends Expr {
    override val toString = {
      val f = (x : String, y : String) => x + ", " + y 
      val x = if (args.isEmpty) "" else args.map{x => x.toString}.reduce(f)
      e.toString + "." + m + "(" + x + ")"
    }
    override val checkType : Env => Option[String] = env => {
      val eT = e.checkType(env)
      if (eT.isEmpty || !env.info.mBody(eT.get).contains(m)) None else {
        val set = env.info.mBody(eT.get)(m)
        if (set.size != 1) None else {
          val meth = env.info.methodMap(set.head).find{x => x.name == m}.get
          val checkArgsNum = meth.parameters.size == args.size
          val checkArgsType = meth.parameters.zip(args)
                                  .map{x => x._2.checkType(env).contains(x._1._1.toString)}
                                  .foldLeft(true)((x, y) => x && y)
          if (checkArgsNum && checkArgsType) Some(meth.returnType.toString) else None
        }
      }
    } 
  }
  
  class MethSuperCall(e : Expr, sup : String, m : String, args : List[Expr]) extends Expr {
    override val toString = {
      val f = (x : String, y : String) => x + ", " + y 
      val x = if (args.isEmpty) "" else args.map{x => x.toString}.reduce(f)
      e.toString + "." + sup + "::" + m + "(" + x + ")"
    }
    override val checkType : Env => Option[String] = env => {
      val eT = e.checkType(env)
      if (eT.isEmpty || !env.info.subType(eT.get, sup) || !env.info.mBody(sup).contains(m)) None else {
        val set = env.info.mBody(sup)(m)
        if (set.size != 1) None else {
          val meth = env.info.methodMap(set.head).find{x => x.name == m}.get
          val checkArgsNum = meth.parameters.size == args.size
          val checkArgsType = meth.parameters.zip(args)
                                  .map{x => x._2.checkType(env).contains(x._1._1.toString)}
                                  .foldLeft(true)((x, y) => x && y)
          if (checkArgsNum && checkArgsType) Some(meth.returnType.toString) else None
        }
      }
    }
  }
  
  lexical.reserved += ("class", "extends", "return", "new")
  lexical.delimiters += ("(", ")", "{", "}", ",", ".", "::", ";")
  
  lazy val lcid = ident ^? { case id if id.charAt(0).isLower => id }
  lazy val ucid = ident ^? { case id if id.charAt(0).isUpper => id }
  
  object Parser {
    val pP : PackratParser[Program] =
      rep(pTD) ~ pE ^^ { case l ~ e => new Program(l, e) }
    val pTD : PackratParser[TraitDef] =
      ("class" ~> ucid) ~ ("extends" ~> repsep(pT, ",") | success(List())) ~ ("{" ~> rep(pMD) <~ "}") ^^
        { case n ~ sups ~ ms => new TraitDef(n, sups, ms) }
    val pMD : PackratParser[MethDef] =
      pT ~ lcid ~ ("(" ~> repsep(pT ~ lcid ^^ { case t ~ n => (t, n) }, ",") <~ ")") ~
        ("{" ~> ("return" ~> pE <~ ";") <~ "}") ^^
          { case t ~ n ~ paras ~ e => new MethDef(t, n, paras, e) }
    val pE : PackratParser[Expr] =
      pE ~ ("." ~> lcid) ~ ("(" ~> repsep(pE, ",") <~ ")") ^^
        { case e ~ m ~ args => new MethCall(e, m, args) } |
      pE ~ ("." ~> ucid) ~ ("::" ~> lcid) ~ ("(" ~> repsep(pE, ",") <~ ")") ^^
        { case e ~ sup ~ m ~ args => new MethSuperCall(e, sup, m, args) } |
      "new" ~> pT <~ ("(" ~ ")") ^^ { n => new Object(n) } |
      lcid ^^ { n => new Var(n) } | "(" ~> pE <~ ")" 
    val pT : PackratParser[Type] = ucid ^^ { n => new Type(n) }
  }
  
  def parse(in : String) : Program = phrase(Parser.pP)(new lexical.Scanner(in)) match {
      case t if t.successful => t.get
      case t                 => scala.sys.error(t.toString)
  }
  def main(args : Array[String]) = {
    val p = parse("class A extends B{A m(B x){return new A();}}" +
        "class B extends C{B n(){return new B();} A p(A x, B y) { return x.m(y.n()); }}" +
        "class C{}" +
        "new A().B::p(new A(), new B())")
    println(p.toString)
    println("-----------")
    println("traits = " + p.env.traits)
    println("directUpMap = " + p.env.directUpMap)
    println("methodMap = " + p.env.methodMap)
    println("infoCheck = " + p.env.infoCheck)
    if (p.env.infoCheck) {
      println("upMap = " + p.env.upMap)
      println("mbody = " + p.env.mBody)
      println(p.expr.toString + " :: " + p.typeChecked(new Env(p.env, Map())))
    }
  }
  
}