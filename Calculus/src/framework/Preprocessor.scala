package framework

import AST._
import CompilationError._

object Preprocessor {
  def createInfo(program: Program): Info = {
    val is: List[TypeDef] = program.is
    val e = program.e
    val table = is.map(x => x.name)
    assert(table.distinct.size == table.size, Message("Pre-processor: interface names conflict."))
    val typeMap = is.map(x => {
      assert(x.sups.distinct.size == x.sups.size, Message("Pre-processor: super types of " + x.name + " conflict."))
      for (y <- x.sups) assert(table.contains(y), TypeNotFound(y))
      x.name -> x.sups}).toMap
    val methodMap = is.map(x => {
      val methods = x.methods.map(y => (y.name, y.update))
      assert(methods.distinct.size == methods.size, Message("Pre-processor: methods of " + x.name + " conflict."))
      x.name -> x.methods}).toMap
    new Info(table, typeMap, methodMap, is.map(x => x.name -> x).toMap)
  }
}