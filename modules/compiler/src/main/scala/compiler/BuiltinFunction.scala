package compiler

enum BuiltinFunction(val name: String, val arity: Int) {
  case Print extends BuiltinFunction("print", 1)

  def index: Short = this.ordinal.toShort
}

object BuiltinFunction {
  def fromName(name: String): Option[BuiltinFunction] =
    BuiltinFunction.values.find(_.name == name)

  def fromIndex(index: Short): Option[BuiltinFunction] =
    BuiltinFunction.values.find(_.index == index)
}
