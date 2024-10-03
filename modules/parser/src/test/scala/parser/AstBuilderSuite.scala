package parser

import java.io.ByteArrayInputStream

class AstBuilderSuite extends munit.FunSuite {
  test("should build the AST for a simple program") {
    val code = """
      |function f(x) {
      |  return 1 + 2 * x - 3;
      |}
      |function main() {
      |  let x = 10;
      |  while (x > 8) {
      |    x = x - 1;
      |  }
      |  let y = 1 + x - f(x);
      |  if (x > 5) {
      |    return 1;
      |  } else {
      |    return 0;
      |  }
      |}
      """.stripMargin

    val actualProgram = AstBuilder.build(
      ByteArrayInputStream(code.getBytes)
    )

    val expectedProgram = Program(
      List(
        FunctionDeclaration(
          Expression.Identifier("f"),
          List(Expression.Identifier("x")),
          Statement.Block(
            List(
              Statement.ReturnStatement(
                Expression.BinaryOperation(
                  Expression.BinaryOperation(
                    Expression.IntLiteral(1),
                    BinaryOperator.Add,
                    Expression.BinaryOperation(
                      Expression.IntLiteral(2),
                      BinaryOperator.Multiply,
                      Expression.Identifier("x")
                    )
                  ),
                  BinaryOperator.Subtract,
                  Expression.IntLiteral(3)
                )
              )
            )
          )
        ),
        FunctionDeclaration(
          Expression.Identifier("main"),
          List(),
          Statement.Block(
            List(
              Statement.LetStatement(
                Expression.Identifier("x"),
                Expression.IntLiteral(10)
              ),
              Statement.WhileStatement(
                Expression.BinaryOperation(
                  Expression.Identifier("x"),
                  BinaryOperator.GreaterThan,
                  Expression.IntLiteral(8)
                ),
                Statement.Block(
                  List(
                    Statement.AssignmentStatement(
                      Expression.Identifier("x"),
                      Expression.BinaryOperation(
                        Expression.Identifier("x"),
                        BinaryOperator.Subtract,
                        Expression.IntLiteral(1)
                      )
                    )
                  )
                )
              ),
              Statement.LetStatement(
                Expression.Identifier("y"),
                Expression.BinaryOperation(
                  Expression.BinaryOperation(
                    Expression.IntLiteral(1),
                    BinaryOperator.Add,
                    Expression.Identifier("x")
                  ),
                  BinaryOperator.Subtract,
                  Expression.FunctionCall(
                    Expression.Identifier("f"),
                    List(Expression.Identifier("x"))
                  )
                )
              ),
              Statement.IfStatement(
                Expression.BinaryOperation(
                  Expression.Identifier("x"),
                  BinaryOperator.GreaterThan,
                  Expression.IntLiteral(5)
                ),
                Statement.Block(
                  List(
                    Statement.ReturnStatement(
                      Expression.IntLiteral(1)
                    )
                  )
                ),
                Some(
                  Statement.Block(
                    List(
                      Statement.ReturnStatement(
                        Expression.IntLiteral(0)
                      )
                    )
                  )
                )
              )
            )
          )
        )
      )
    )

    assertEquals(actualProgram, expectedProgram)
  }
}
