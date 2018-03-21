package framework

import AST._
import CompilationError._

object Preprocessor {
  def createInfo(program: Program): Info = {
    val is = program.is
    val e = program.e
    val table = is.map(x => x.name)
    assert(table.distinct.size == table.size, Message("Pre-processor: interface names conflict."))
    val typeMap = is.map(x => {
      assert(x.sups.distinct.size == x.sups.size, Message("Pre-processor: super types of " + x.name + " conflict."))
      for (y <- x.sups) assert(table.contains(y), TypeNotFound(y))
      x.name -> x.sups}).toMap
    val methodMap = is.map(x => {
      var updateMap: Map[String, List[String]] = Map()
      for (eachMethod <- x.methods) {
        val mName = eachMethod.name
        val mUpdate = eachMethod.update
        updateMap += mName -> (updateMap.getOrElse(mName, List()) ++ mUpdate)
      }
      for (value <- updateMap.values) {
        assert(value.distinct.size == value.size, Message("Pre-processor: multiple updates conflict in " + x.name + "."))
        assert(value.size <= 1 || !value.contains(x.name),
            Message("Pre-processor: multiple updates of " + x.name + " containing itself."))
      }
    x.name -> x.methods}).toMap
    new Info(table, typeMap, methodMap, is.map(x => x.name -> x).toMap)
  }
}