package vm

import compiler.{Compiler, Disassembler}
import parser.AstBuilder

import java.io.{ByteArrayInputStream, ByteArrayOutputStream, PrintStream}

class VMSuite extends munit.FunSuite {
  check(
    "should handle binary expressions",
    """
        |function main() {
        |  let x = 20;
        |  let y = 10;
        |  let _ = print(x + y);
        |  let _ = print(x - y);
        |  let _ = print(x * y);
        |  let _ = print(x / y);
        |  let _ = print(x == y);
        |  let _ = print(x != y);
        |  let _ = print(x > y);
        |  let _ = print(x < y);
        |  let _ = print(x >= y);
        |  let _ = print(x <= y);
        |}
        """.stripMargin,
    "30\n10\n200\n2\n0\n1\n1\n0\n1\n0\n"
  )

  check(
    "should handle variable assignments",
    """
        |function main() {
        |  let x = 10;
        |  x = x + 1;
        |  let _ = print(x);
        |}
        """.stripMargin,
    "11\n"
  )

  check(
    "should handle if statements",
    """
        |function main() {
        |  let x = 10;
        |  if (x > 5) {
        |    let _ = print(1);
        |  } else {
        |    let _ = print(0);
        |  }
        |}
        """.stripMargin,
    "1\n"
  )

  check(
    "should handle while loops",
    """
        |function main() {
        |  let x = 10;
        |  while (x > 5) {
        |    x = x - 1;
        |  }
        |  let _ = print(x);
        |}
        """.stripMargin,
    "5\n"
  )

  check(
    "should handle function calls",
    """
        |function f(x) {
        |  return x + 1;
        |}
        |function main() {
        |  let x = 10;
        |  let y = f(x);
        |  let _ = print(y);
        |}
        """.stripMargin,
    "11\n"
  )

  check(
    "should interpret a simple program",
    """
      |function f(x, y) {
      |  return x + y;
      |}
      |function main() {
      |  let x = 10;
      |  while (x < 15) {
      |    let w = 1;
      |    x = x + w;
      |  }
      |  if (x == 15) {
      |    let y = 99;
      |    x = 18;
      |  }
      |  let y = 20;
      |  let _ = print(f(x, y));
      |}
      """.stripMargin,
    "38\n"
  )

  private def check(
      name: String,
      code: String,
      expected: String
  )(implicit loc: munit.Location): Unit = test(name) {
    val ast = AstBuilder.build(ByteArrayInputStream(code.getBytes))
    val compiledProgram = Compiler.compileProgram(ast)

    val disassembled = Disassembler.disassemble(compiledProgram)

    val outputStream = ByteArrayOutputStream()
    val vm = VM(PrintStream(outputStream))
    vm.run(compiledProgram)

    assertEquals(
      outputStream.toString,
      expected,
      s"Output does not match. Disassembled code:\n\n$disassembled"
    )
  }
}
