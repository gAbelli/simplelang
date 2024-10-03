package compiler

import compiler.Compiler.CompiledProgram

object Disassembler {
  def disassemble(compiledProgram: CompiledProgram): String = {
    val builder = StringBuilder()

    for ((chunk, chunkIndex) <- compiledProgram.chunks.zipWithIndex) {
      builder.append(
        s"function ${compiledProgram.functionsMetadata(chunkIndex).name}:\n"
      )
      var i = 0
      while (i < chunk.size) {
        val instruction = Instruction.fromOpCode(chunk(i)).get
        builder.append(instruction.toString)
        instruction match {
          case Instruction.Constant | Instruction.GetLocal | Instruction.SetLocal |
              Instruction.Jump | Instruction.JumpIfFalse => {
            builder.append(s" ${chunk(i + 1)}")
            i += 2
          }
          case Instruction.Call => {
            val functionIndex = chunk(i + 1)
            val functionName = compiledProgram.functionsMetadata(functionIndex).name
            builder.append(s" $functionName")
            i += 2
          }
          case Instruction.CallBuiltin => {
            val builtinIndex = chunk(i + 1)
            val builtinFunction = BuiltinFunction
              .fromIndex(builtinIndex)
              .getOrElse(
                throw IllegalArgumentException(
                  s"Unknown builtin function $builtinIndex"
                )
              )
            builder.append(s" ${builtinFunction.name}")
            i += 2
          }
          case Instruction.Pop | Instruction.Add | Instruction.Subtract | Instruction.Multiply |
              Instruction.Divide | Instruction.Equal | Instruction.NotEqual |
              Instruction.GreaterThan | Instruction.LessThan | Instruction.GreaterThanOrEqual |
              Instruction.LessThanOrEqual | Instruction.Return => {
            i += 1
          }
        }
        builder.append('\n')
      }
      builder.append('\n')
    }

    builder.toString()
  }
}
