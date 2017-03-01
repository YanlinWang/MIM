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
    val methodMap = mMap // TO BE DEBUGGED, ALL CODE RELATED TO methodMap
    val methodNameMap = mMap.mapValues{x => x.filter{y => y.updateTrait.isEmpty()}.map{y => y.name}}
    val noOverloading : Boolean = noOL // TO BE DEBUGGED
    lazy val upMapOption : Option[Map[String, Set[String]]] = {
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
    lazy val noCycle : Boolean = upMapOption.isDefined
    lazy val infoCheck : Boolean = noCycle && noOverloading
    lazy val upMap : Map[String, Set[String]] = upMapOption.getOrElse(Map())
    lazy val mBody : Map[String, Map[String, Set[String]]] = {
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
      new Info(traits ++ env.traits,
          directUpMap ++ env.directUpMap,
          methodMap ++ env.methodMap,
          noOverloading && env.noOverloading)
    }
    def subType(s1 : String, s2 : String) : Boolean = s1 == s2 || upMap.get(s1).get(s2)
  }
  
  type VarEnv = Map[String, String]
  class Env(i : Info, v : VarEnv, t : Option[String]) {
    val info : Info = i
    val vars : VarEnv = v
    val thisTrait : Option[String] = t
    def addVar(x : (Type, String)) : Env =
      new Env(info, vars + (x._2 -> x._1.toString), t)
    def addVars(xs : List[(Type, String)]) : Env =
      new Env(info, vars ++ xs.map{x => (x._2, x._1.toString)}.toMap, t)
  }
  
  class Program(l : List[TraitDef], e : Expr) {
    val expr = e
    override val toString = {
      val f = (x : String, y : String) => x + "\n" + y
      val x = if (l.isEmpty) "" else l.map{x => x.toString}.reduce(f) + "\n"
      x + e.toString
    }
    val env : Option[Info] = {
      val res = l.foldLeft(new Info(Set(), Map(), Map(), true))((x, y) => x.merge(y.env))
      res.methodMap.forall(p => {
        res.upMap(p._1).forall(up => {
          val methods2 = res.methodMap(up)
          p._2.forall{meth => !methods2.exists{meth2 => meth.name == meth2.name && !meth.canOverride(meth2)}}})
      })
      if (res.infoCheck && res.methodMap.forall(p => {
          res.upMap(p._1).forall(up => {
            val methods2 = res.methodMap(up)
            p._2.forall{meth => !methods2.exists{meth2 => meth.name == meth2.name && !meth.canOverride(meth2)}}})
        }) && res.mBody.forall(p => p._2.forall(q => q._2.size > 0)) &&
        l.forall{x => x.typeChecked(new Env(res, Map(), Some(x.name)))}) Some(res) else None
    }
    val checkType : Option[String] = if (env.isEmpty) None else e.checkType(new Env(env.get, Map(), None))
    val eval : Option[Value] = if (checkType.isDefined) e.eval(new Env(env.get, Map(), None)) else None
  }
  
  class TraitDef(n : String, sups : List[Type], ms : List[MethDef]) {
    val name = n
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
  
  class MethDef(t : Type, n : String, paras : List[(Type, String)], update : String, e : Expr) {
    val returnType = t
    val name = n
    val parameters = paras
    val updateTrait = update
    val returnExpr = e
    override val toString = {
      val f = (x : String, y : String) => x + ", " + y 
      val x = if (paras.isEmpty) "" else paras.map{x => x._1.toString + " " + x._2}.reduce(f)
      val updt = if (update.isEmpty()) "" else " override " + update
      t.toString + " " + n + "(" + x + ")" + updt + " { return " + e.toString + "; }"
    }
    val typeChecked : Env => Boolean = env => {
      noDuplicate(paras.map{x => x._2}) &&
      paras.forall{p => p._1.isBounded(env)} &&
      (update.isEmpty() || (env.thisTrait.isDefined && env.thisTrait.get != update
          && env.info.subType(env.thisTrait.get, update)
          && !env.info.methodMap.get(update).get
                 .filter{meth => meth.name == n && meth.updateTrait.isEmpty() && canOverride(meth)}.isEmpty)) &&
      e.checkType(env.addVars(paras)).exists{x => env.info.subType(x, t.toString)}
    }
    def canOverride(m : MethDef) : Boolean =
      returnType.toString == m.returnType.toString &&
      parameters.size == m.parameters.size &&
      parameters.zip(m.parameters).forall(p => p._1._1.toString == p._2._1.toString)
  }
  
  class Type(n : String) {
    override val toString = n
    val isBounded : Env => Boolean = e => e.info.traits(n)
  }
  
  class Expr {
    val checkType : Env => Option[String] = _ => None
    val eval : Env => Option[Value] = _ => None
    var staticType : Option[String] = None
    def trans(map : Map[String, Expr]) : Expr = this
  }
  
  class Var(n : String) extends Expr {
    override val toString = n
    override def trans(map : Map[String, Expr]) = map.get(n).get
    override val checkType : Env => Option[String] = env => env.vars.get(n)
  }
  
  class Object(n : Type) extends Expr  {
    override val toString = "new " + n.toString + "()"
    override val checkType : Env => Option[String] = env =>
      if (env.info.traits(n.toString)) Some(n.toString) else None
    override val eval : Env => Option[Value] = _ => Some(new ObjValue(n.toString))
  }
  
  class MethCall(e : Expr, m : String, args : List[Expr]) extends Expr {
    override val toString = {
      val f = (x : String, y : String) => x + ", " + y 
      val x = if (args.isEmpty) "" else args.map{x => x.toString}.reduce(f)
      e.toString + "." + m + "(" + x + ")"
    }
    override def trans(map : Map[String, Expr]) = new MethCall(e.trans(map), m, args.map{x => x.trans(map)})
    override val checkType : Env => Option[String] = env => {
      val eT = e.checkType(env)
      if (eT.isEmpty || !env.info.mBody(eT.get).contains(m)) None else {
        val set = env.info.mBody(eT.get)(m)
        if (set.size != 1) None else {
          val meth = env.info.methodMap(set.head).find{x => x.name == m}.get
          val checkArgsNum = meth.parameters.size == args.size
          val checkArgsType = meth.parameters.zip(args)
                                  .map{x => x._2.checkType(env).exists{t => env.info.subType(t, x._1._1.toString)}}
                                  .foldLeft(true)((x, y) => x && y)
          if (checkArgsNum && checkArgsType) Some(meth.returnType.toString) else None
        }
      }
    }
    override val eval : Env => Option[Value] = env => {
      val eVal = e.eval(env)
      val argsVal = args.map{x => x.eval(env)}
      if (eVal.isEmpty || argsVal.exists{x => x.isEmpty}) None else {
        val dynamicT = eVal.get.dynamicType
        val staticT = if (e.staticType.isEmpty) e.checkType(env).get else e.staticType.get
        val set = env.info.mBody(dynamicT)(m)
        val specific = if (set.size == 1) Some(set.head)
                       else if (set.size > 1 && set.contains(staticT)) Some(staticT)
                       else None
        if (specific.isEmpty) None else {
          val sets = env.info.methodMap.get(dynamicT).get.find{x => x.name == m && x.updateTrait == specific.get}
          val meth = if (sets.isEmpty) env.info.methodMap.get(specific.get).get
                                          .find{x => x.name == m && x.updateTrait.isEmpty()}.get
                     else sets.get
          meth.returnExpr.trans(meth.parameters.zip(argsVal.map{x => {
            val expr : Expr = new Object(new Type(x.get.dynamicType))
            expr}}).map{x => (x._1._2, {val expr = x._2; expr.staticType = Some(x._1._1.toString); expr})}.toMap).eval(env)
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
    override def trans(map : Map[String, Expr]) = new MethSuperCall(e.trans(map), sup, m, args.map{x => x.trans(map)})
    override val checkType : Env => Option[String] = env => {
      val eT = e.checkType(env)
      if (eT.isEmpty || !env.info.subType(eT.get, sup) || !env.info.mBody(sup).contains(m)) None else {
        val set = env.info.mBody(sup)(m)
        if (set.size != 1) None else {
          val meth = env.info.methodMap(set.head).find{x => x.name == m}.get
          val checkArgsNum = meth.parameters.size == args.size
          val checkArgsType = meth.parameters.zip(args)
                                  .map{x => x._2.checkType(env).exists{t => env.info.subType(t, x._1._1.toString)}}
                                  .foldLeft(true)((x, y) => x && y)
          if (checkArgsNum && checkArgsType) Some(meth.returnType.toString) else None
        }
      }
    }
    override val eval : Env => Option[Value] = env => {
      val eVal = e.eval(env)
      val argsVal = args.map{x => x.eval(env)}
      if (eVal.isEmpty || argsVal.exists{x => x.isEmpty}) None else {
        val dynamicT = eVal.get.dynamicType
        val staticT = sup
        val set = env.info.mBody(dynamicT)(m)
        val specific = if (set.size == 1) Some(set.head)
                       else if (set.size > 1 && set.contains(staticT)) Some(staticT)
                       else None
        if (specific.isEmpty) None else {
          val sets = env.info.methodMap.get(dynamicT).get.find{x => x.name == m && x.updateTrait == specific.get}
          val meth = if (sets.isEmpty) env.info.methodMap.get(specific.get).get
                                          .find{x => x.name == m && x.updateTrait.isEmpty()}.get
                     else sets.get
          meth.returnExpr.trans(meth.parameters.zip(argsVal.map{x => {
            val expr : Expr = new Object(new Type(x.get.dynamicType))
            expr}}).map{x => (x._1._2, {val expr = x._2; expr.staticType = Some(x._1._1.toString); expr})}.toMap).eval(env)
        }
      }
    }
  }
  
  class SuperCall(sup : String, m : String, args : List[Expr]) extends Expr {
    override val toString = {
      val f = (x : String, y : String) => x + ", " + y 
      val x = if (args.isEmpty) "" else args.map{x => x.toString}.reduce(f)
      "super." + sup + "::" + m + "(" + x + ")"
    }
    override def trans(map : Map[String, Expr]) = new SuperCall(sup, m, args.map{x => x.trans(map)})
    override val checkType : Env => Option[String] = env => {
      val thisT = env.thisTrait
      if (thisT.isEmpty || !env.info.subType(thisT.get, sup) || !env.info.mBody(sup).contains(m)) None else {
        val set = env.info.mBody(sup)(m)
        if (set.size != 1) None else {
          val meth = env.info.methodMap(set.head).find{x => x.name == m}.get
          val checkArgsNum = meth.parameters.size == args.size
          val checkArgsType = meth.parameters.zip(args)
                                  .map{x => x._2.checkType(env).exists{t => env.info.subType(t, x._1._1.toString)}}
                                  .foldLeft(true)((x, y) => x && y)
          if (checkArgsNum && checkArgsType) Some(meth.returnType.toString) else None
        }
      }
    }
    override val eval : Env => Option[Value] = env => {
      val argsVal = args.map{x => x.eval(env)}
      if (argsVal.exists{x => x.isEmpty}) None else {
        val set = env.info.mBody(sup)(m)
        val specific = if (set.size == 1) Some(set.head) else None
        if (specific.isEmpty) None else {
          val sets = env.info.methodMap.get(sup).get.find{x => x.name == m && x.updateTrait == specific.get}
          val meth = if (sets.isEmpty) env.info.methodMap.get(specific.get).get
                                          .find{x => x.name == m && x.updateTrait.isEmpty()}.get
                     else sets.get
          meth.returnExpr.trans(meth.parameters.zip(argsVal.map{x => {
            val expr : Expr = new Object(new Type(x.get.dynamicType))
            expr}}).map{x => (x._1._2, {val expr = x._2; expr.staticType = Some(x._1._1.toString); expr})}.toMap).eval(env)
        }
      }
    }
  }
  
  class Value {
    val dynamicType = ""
  }
  
  class ObjValue(n : String) extends Value {
    val name = n
    override val toString = n
    override val dynamicType = n
  }
  
  lexical.reserved += ("interface", "extends", "return", "new", "override", "super")
  lexical.delimiters += ("(", ")", "{", "}", ",", ".", "::", ";")
  
  lazy val lcid = ident ^? { case id if id.charAt(0).isLower => id }
  lazy val ucid = ident ^? { case id if id.charAt(0).isUpper => id }
  
  object Parser {
    val pP : PackratParser[Program] =
      rep(pTD) ~ pE ^^ { case l ~ e => new Program(l, e) }
    val pTD : PackratParser[TraitDef] =
      ("interface" ~> ucid) ~ ("extends" ~> repsep(pT, ",") | success(List())) ~ ("{" ~> rep(pMD) <~ "}") ^^
        { case n ~ sups ~ ms => new TraitDef(n, sups, ms) }
    val pMD : PackratParser[MethDef] =
      pT ~ lcid ~ ("(" ~> repsep(pT ~ lcid ^^ { case t ~ n => (t, n) }, ",") <~ ")") ~
        ("override" ~> ucid | success("")) ~ ("{" ~> ("return" ~> pE <~ ";") <~ "}") ^^
          { case t ~ n ~ paras ~ update ~ e => new MethDef(t, n, paras, update, e) }
    val pE : PackratParser[Expr] =
      pE ~ ("." ~> lcid) ~ ("(" ~> repsep(pE, ",") <~ ")") ^^
        { case e ~ m ~ args => new MethCall(e, m, args) } |
      pE ~ ("." ~> ucid) ~ ("::" ~> lcid) ~ ("(" ~> repsep(pE, ",") <~ ")") ^^
        { case e ~ sup ~ m ~ args => new MethSuperCall(e, sup, m, args) } |
      (("super" ~ ".") ~> ucid) ~ ("::" ~> lcid) ~ ("(" ~> repsep(pE, ",") <~ ")") ^^
        { case sup ~ m ~ args => new SuperCall(sup, m, args) } |
      "new" ~> pT <~ ("(" ~ ")") ^^ { n => new Object(n) } |
      lcid ^^ { n => new Var(n) } | "(" ~> pE <~ ")" 
    val pT : PackratParser[Type] = ucid ^^ { n => new Type(n) }
  }
  
  def parse(in : String) : Program = phrase(Parser.pP)(new lexical.Scanner(in)) match {
      case t if t.successful => t.get
      case t                 => scala.sys.error(t.toString)
  }
  def main(args : Array[String]) = {
//    val p = parse("interface A{}" +
//        "interface B extends A{B n(){return new B();} C p(C x, B y) { return x.m(y.n()); }}" +
//        "interface C extends B{C m(B x){return new C();} B n()override B{return new C();}}" +
//        "new C().n()")
//        "new C().B::p(new C(), new B())")
//    val p = parse("interface A { D m() {return new E();}}" +
//        "interface B {D m() {return new D();}}" +
//        "interface C extends A, B { D func(A a) { return a.m(); }}" +
//        "interface D {} interface E extends D {}" + " new C().func(new C())")
        
    val p = parse(
"interface P {} " +
"interface P1 extends P {} " +
"interface P2 extends P {} " +
"interface P3 extends P {} " +
"interface P4 extends P {} " +
"interface P5 extends P {} " +
"interface P6 extends P {} " +
"interface P7 extends P {} " +
"interface A { " +
"  P m() { return new P1(); } " +
"} " +
"interface B { " +
"  P m() { return new P2(); } " +
"} " +
"interface C extends A, B { " +
"  P m() { return new P3(); } " +
"} " +
"interface D extends C, A { " +
"} " +
"new C().A::m() ");


    println(p.toString)
    println("-----------")
    var str = "Type-check failed!"
    if (p.env.isDefined && p.checkType.isDefined) {
      val t = p.checkType
      if (t.isDefined) str = p.expr.toString + " :: " + t.get
    }
    println(str)
    println("eval = " + p.eval.toString)
  }
  
}