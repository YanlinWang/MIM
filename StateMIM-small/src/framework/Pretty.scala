package framework

import AST._

object Pretty {
  def pretty(p: Program): String = {
    val isStr = if (p.is.isEmpty) "" else p.is.map(pretty(_)).reduce((a, b) => a + "\n" + b) + "\n\n"
    isStr + pretty(p.e)
  }
  def pretty(t: TypeDef): String = {
    val supStr = if (t.sups.isEmpty) "" else " extends " + t.sups.reduce((a, b) => a + ", " + b)
    val methodsStr = if (t.methods.size == 0) "{}" else "{\n" + t.methods.map(pretty(_)).reduce((a, b) => a + "\n" + b) + "\n}"
    "interface " + t.name + supStr + " " + methodsStr
  }
  def pretty(m: MethDef): String = {
      val parasStr = if (m.paras.isEmpty) "" else m.paras.map(p => p.paramType + " " + p.name).reduce((a, b) => a + ", " + b)
      val body = if (m.returnExpr.isEmpty) ";" else " {\n    return " + pretty(m.returnExpr.get) + ";\n  }"
      "  " + m.returnType + " " + m.name + "(" + parasStr + ") override " + m.update + body
  }
  def pretty(e: Expr): String = e match {
    case Var(n) => n
    case Invk(e, m, args) => {
      val argsStr = if (args.isEmpty) "" else args.map(pretty(_)).reduce((a, b) => a + ", " + b)
      pretty(e) + "." + m + "(" + argsStr + ")"
    }
    case AnnoExpr(i, e) => "((" + i + ") " + pretty(e) + ")"
    case _ => e.toString() // TODO
  }
}