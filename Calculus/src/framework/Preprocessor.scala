package framework

import AST._

object Preprocessor {
  def createInfo(program: Program): Info = {
    val is: List[TypeDef] = program.is
    val e = program.e
    val table = is.map(x => x.name)
    assert(table.distinct.size == table.size, "Pre-processor: class name conflicts.")
    val typeMap = is.map(x => {
      assert(x.sups.distinct.size == x.sups.size, "Pre-processor: super types of " + x.name + " conflict.")
      assert(x.sups.forall(y => table.contains(y)), "Pre-processor: super types of " + x.name + " undefined.")
      x.name -> x.sups}).toMap
    val methodMap = is.map(x => {
      val methods = x.methods.map(y => (y.name, y.update))
      assert(methods.distinct.size == methods.size, "Pre-processor: methods of " + x.name + " conflict.")
      x.name -> x.methods}).toMap
    new Info(table, typeMap, methodMap, is.map(x => x.name -> x).toMap)
  }
}