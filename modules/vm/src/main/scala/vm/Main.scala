package vm

import compiler.Compiler
import parser.AstBuilder

import java.io.{FileInputStream, PrintStream}

object Main {
  def main(args: Array[String]): Unit = {
    if (args.length != 1) {
      println("Usage: vm <file>")
      return
    }

    val file = args(0)
    val code = new FileInputStream(file)

    val ast = AstBuilder.build(code)
    val compiledProgram = Compiler.compileProgram(ast)

    val outputStream = new PrintStream(System.out)
    val vm = new VM(outputStream)
    vm.run(compiledProgram)
  }
}
