package framework

import scala.util.parsing.combinator._
import scala.util.parsing.combinator.syntactical._

object StateMIMParser extends StandardTokenParsers with PackratParsers {
  import AST._
  
  lexical.reserved += ("interface", "extends", "return", "override", "super", "static", "let")
  lexical.delimiters += ("(", ")", "{", "}", ",", ".", "::", ";", "=")
  
  val lcid = ident ^? { case id if id.charAt(0).isLower => id }
  val ucid = ident ^? { case id if id.charAt(0).isUpper => id }
  
  object Parser {
    val pField: PackratParser[Field] = ucid ~ ucid ~ ("." ~> lcid) ^^ {case x1 ~ x2 ~ x3 => Field(x1, x2, x3) }
    val pMC: PackratParser[Constructor] = ("static" ~> ucid) ~ lcid ~ ("(" ~>
      repsep(pField, ",") <~ ")") ~ ";" ^^ { case i ~ m ~ paras ~ j => new Constructor(i, m, paras) } 
    val pParameter: PackratParser[Parameter] = ucid ~ lcid ^^ { case t ~ n => Parameter(t, n) }
    
    val pP : PackratParser[Program] = rep(pTD) ~ pE ^^ { case l ~ e => new Program(l, e) }
    val pTD : PackratParser[TypeDef] =
      ("interface" ~> ucid) ~ ("extends" ~> repsep(ucid, ",") | success(List())) ~ ("{" ~> ((pMC ^^ {case x => Some(x)} | success(None)) ~ rep(pMD)) <~ "}") ^^
        { case n ~ sups ~ (constr ~ ms) => new TypeDef(n, sups, ms, constr) }
    val pMD : PackratParser[MethDef] =
      ucid ~ lcid ~ ("(" ~> repsep(pParameter, ",") <~ ")") ~
        ("override" ~> ucid) ~ ("{" ~> ("return" ~> pE <~ ";") <~ "}" ^^ { case pE => Some(pE) } | ";" ^^^ None) ^^
          { case t ~ m ~ paras ~ update ~ e => new MethDef(t, m, paras, update, e) }
    val pE : PackratParser[Expr] =
      pE ~ ("." ~> lcid) ~ ("(" ~> repsep(pE, ",") <~ ")") ^^
        { case e ~ m ~ args => Invk(e, m, args) } |||
      pE ~ ("." ~> ucid) ~ ("(" ~> pE <~ ")") ^^
        { case e ~ m ~ arg if (m.startsWith("SET_")) => InvkSetter(e, m.substring(4), arg) } |||
      ("let" ~> ucid) ~ lcid ~ ("=" ~> pE) ~ (";" ~> pE) ^^
        { case t ~ x ~ e1 ~ e2 => LetExpr(t, x, e1, e2) } |||
      lcid ~ ("(" ~> repsep(pE, ",") <~ ")") ^^
        { case m ~ args => Invk(Var("this"), m, args) } |||
      ("(" ~> ucid <~ ")") ~ pE ^^ { case i ~ e => AnnoExpr(i, e) } |||
      lcid ^^ Var ||| "(" ~> pE <~ ")" ||| 
      ucid ~ ("." ~> lcid) ~ ("(" ~> repsep(pE, ",") <~ ")") ^^
        { case t ~ m ~ args => InvkStatic(t, m, args) }
  }
  
  def parse(in: String): Either[String, Program] = phrase(Parser.pP)(new lexical.Scanner(in)) match {
      case t if t.successful => Right(t.get)
      case t                 => Left(t.toString)
  }
}