package compiler

enum Instruction {
  case Constant
  case Pop
  case GetLocal
  case SetLocal
  case Add
  case Subtract
  case Multiply
  case Divide
  case Equal
  case NotEqual
  case GreaterThan
  case LessThan
  case GreaterThanOrEqual
  case LessThanOrEqual
  case Call
  case CallBuiltin
  case Jump
  case JumpIfFalse
  case Return

  def opCode: Short = this.ordinal.toShort
}

object Instruction {
  def fromOpCode(opCode: Short): Option[Instruction] =
    Instruction.values.find(_.opCode == opCode)
}
