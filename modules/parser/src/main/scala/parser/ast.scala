package parser

enum Expression {
  case IntLiteral(value: Int)
  case Identifier(name: String)
  case BinaryOperation(left: Expression, op: BinaryOperator, right: Expression)
  case FunctionCall(id: Identifier, args: List[Expression])
}

enum BinaryOperator {
  case Add, Subtract, Multiply, Divide,
    GreaterThan, LessThan,
    GreaterThanOrEqual, LessThanOrEqual,
    Equal, NotEqual
}

enum Statement {
  case LetStatement(id: Expression.Identifier, expr: Expression)
  case IfStatement(
      condition: Expression,
      thenBlock: Block,
      elseBlock: Option[Block]
  )
  case WhileStatement(condition: Expression, body: Block)
  case AssignmentStatement(id: Expression.Identifier, expr: Expression)
  case ReturnStatement(expr: Expression)
  case Block(statements: List[Statement])
}

case class FunctionDeclaration(
    id: Expression.Identifier,
    params: List[Expression.Identifier],
    body: Statement.Block
)

case class Program(functions: List[FunctionDeclaration])
