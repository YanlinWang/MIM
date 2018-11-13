package framework

import scala.util.parsing.combinator._
import scala.util.parsing.combinator.syntactical._

object StateMIMParser extends StandardTokenParsers with PackratParsers {
  import AST._
  
  lexical.reserved += ("class", "extends", "return", "override", "new") // super?
  lexical.delimiters += ("(", ")", "{", "}", ",", ".", ";", "=")
  
  val lcid = ident ^? { case id if id.charAt(0).isLower => id }
  val ucid = ident ^? { case id if id.charAt(0).isUpper => id }
  
  object Parser {
    val pField: PackratParser[Field] = ucid ~ ucid ~ ("." ~> lcid) ^^ {case x1 ~ x2 ~ x3 => Field(x1, x2, x3) }
    val pMC: PackratParser[Constructor] = ("new" ~ "(") ~> repsep(pField, ",") <~ (")" ~ ";") ^^ { case paras => new Constructor(paras) } 
    val pParameter: PackratParser[Parameter] = ucid ~ lcid ^^ { case t ~ n => Parameter(t, n) }
    
    val pP : PackratParser[Program] = rep(pTD) ~ pE ^^ { case l ~ e => new Program(l, e) }
    val pTD : PackratParser[TypeDef] =
      ("class" ~> ucid) ~ ("extends" ~> repsep(ucid, ",") | success(List())) ~ ("{" ~> ((pMC ^^ {case x => Some(x)} | success(None)) ~ rep(pMD)) <~ "}") ^^
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
      pE ~ ("." ~> ucid) ~ ("(" ~> pE <~ ")") ~ (";" ~> pE) ^^
        { case e ~ m ~ arg ~ e2 if (m.startsWith("SET_")) => LetExpr("Void", "TEMP", InvkSetter(e, m.substring(4), arg), e2) } |||
      ucid ~ lcid ~ ("=" ~> pE) ~ (";" ~> pE) ^^
        { case t ~ x ~ e1 ~ e2 => LetExpr(t, x, e1, e2) } |||
      lcid ~ ("(" ~> repsep(pE, ",") <~ ")") ^^
        { case m ~ args => Invk(Var("this"), m, args) } |||
      ("(" ~> ucid <~ ")") ~ pE ^^ { case i ~ e => AnnoExpr(i, e) } |||
      lcid ^^ Var ||| "(" ~> pE <~ ")" ||| 
      ("new" ~> ucid) ~ ("(" ~> repsep(pE, ",") <~ ")") ^^
        { case t ~ args => InvkStatic(t, args) }
  }
  
  def parse(in: String): Either[String, Program] = {
    val prelude = "class Void {new();}"
    phrase(Parser.pP)(new lexical.Scanner(prelude + in)) match {
      case t if t.successful => Right(t.get)
      case t                 => Left(t.toString)
    }
  }
}