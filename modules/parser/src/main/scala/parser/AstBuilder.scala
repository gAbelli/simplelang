package parser

import org.antlr.v4.runtime.*
import parser.SimpleLangParser.*

import java.io.InputStream
import scala.jdk.CollectionConverters.*

object AstBuilder {
  def build(code: InputStream): Program = {
    val charStream = CharStreams.fromStream(code)

    val lexer = SimpleLangLexer(charStream)
    val tokens = CommonTokenStream(lexer)
    val parser = SimpleLangParser(tokens)

    val tree = parser.program()

    buildProgram(tree)
  }

  private def buildProgram(ctx: SimpleLangParser.ProgramContext): Program = {
    val functions = ctx.functionDecl().asScala.map(buildFunctionDeclaration).toList
    Program(functions)
  }

  private def buildFunctionDeclaration(
      ctx: SimpleLangParser.FunctionDeclContext
  ): FunctionDeclaration = {
    val id: Expression.Identifier = Expression.Identifier(ctx.ID().getText)
    val params = Option(ctx.paramList()).toList.flatMap { pl =>
      pl.ID()
        .asScala
        .map(id => Expression.Identifier(id.getText): Expression.Identifier)
        .toList
    }
    val body = buildBlock(ctx.block())
    FunctionDeclaration(id, params, body)
  }

  private def buildBlock(
      ctx: SimpleLangParser.BlockContext
  ): Statement.Block = {
    val stmts = ctx.stmtList().stmt().asScala.map(buildStatement).toList
    Statement.Block(stmts)
  }

  private def buildStatement(ctx: SimpleLangParser.StmtContext): Statement = {
    if (ctx.letStmt() != null) {
      val letDecl = ctx.letStmt()
      val id: Expression.Identifier = Expression.Identifier(letDecl.ID().getText)
      val expr = buildExpression(letDecl.expr())
      Statement.LetStatement(id, expr)
    } else if (ctx.returnStmt() != null) {
      val returnStmt = ctx.returnStmt()
      val expr = buildExpression(returnStmt.expr())
      Statement.ReturnStatement(expr)
    } else if (ctx.ifStmt() != null) {
      val ifStmt = ctx.ifStmt()
      val condition = buildExpression(ifStmt.expr())
      val thenBlock = buildBlock(ifStmt.block(0))
      val elseBlock = if (ifStmt.block().size > 1) Some(buildBlock(ifStmt.block(1))) else None
      Statement.IfStatement(condition, thenBlock, elseBlock)
    } else if (ctx.whileStmt() != null) {
      val whileStmt = ctx.whileStmt()
      val condition = buildExpression(whileStmt.expr())
      val body = buildBlock(whileStmt.block())
      Statement.WhileStatement(condition, body)
    } else if (ctx.assignmentStmt() != null) {
      val assignment = ctx.assignmentStmt()
      val id: Expression.Identifier = Expression.Identifier(assignment.ID().getText)
      val expr = buildExpression(assignment.expr())
      Statement.AssignmentStatement(id, expr)
    } else if (ctx.block() != null) {
      buildBlock(ctx.block())
    } else {
      throw IllegalArgumentException("Unknown statement type")
    }
  }

  private def buildExpression(ctx: SimpleLangParser.ExprContext): Expression =
    buildRelationalExpression(ctx.relationalExpr())

  private def buildRelationalExpression(
      ctx: SimpleLangParser.RelationalExprContext
  ): Expression = {
    val left = buildAdditiveExpression(ctx.additiveExpr(0))

    var currentExpr = left

    for (i <- 1 until ctx.additiveExpr().size()) {
      val op = ctx.getChild(2 * i - 1).getText
      val right = buildAdditiveExpression(ctx.additiveExpr(i))
      currentExpr = Expression.BinaryOperation(
        currentExpr,
        op match {
          case ">"  => BinaryOperator.GreaterThan
          case "<"  => BinaryOperator.LessThan
          case ">=" => BinaryOperator.GreaterThanOrEqual
          case "<=" => BinaryOperator.LessThanOrEqual
          case "==" => BinaryOperator.Equal
          case "!=" => BinaryOperator.NotEqual
          case _    => throw IllegalArgumentException("Unknown operator")
        },
        right
      )
    }
    currentExpr
  }

  private def buildAdditiveExpression(
      ctx: SimpleLangParser.AdditiveExprContext
  ): Expression = {
    val left = buildMultiplicativeExpression(ctx.multiplicativeExpr(0))

    var currentExpr = left

    for (i <- 1 until ctx.multiplicativeExpr().size()) {
      val op = ctx.getChild(2 * i - 1).getText
      val right = buildMultiplicativeExpression(ctx.multiplicativeExpr(i))
      currentExpr = Expression.BinaryOperation(
        currentExpr,
        op match {
          case "+" => BinaryOperator.Add
          case "-" => BinaryOperator.Subtract
          case _   => throw IllegalArgumentException("Unknown operator")
        },
        right
      )
    }
    currentExpr
  }

  private def buildMultiplicativeExpression(
      ctx: SimpleLangParser.MultiplicativeExprContext
  ): Expression = {
    val left = buildPrimaryExpression(ctx.primaryExpr(0))

    var currentExpr = left

    for (i <- 1 until ctx.primaryExpr().size()) {
      val op = ctx.getChild(2 * i - 1).getText
      val right = buildPrimaryExpression(ctx.primaryExpr(i))
      currentExpr = Expression.BinaryOperation(
        currentExpr,
        op match {
          case "*" => BinaryOperator.Multiply
          case "/" => BinaryOperator.Divide
          case _   => throw IllegalArgumentException("Unknown operator")
        },
        right
      )
    }
    currentExpr
  }

  private def buildPrimaryExpression(
      ctx: SimpleLangParser.PrimaryExprContext
  ): Expression = {
    ctx match {
      case number if number.INT() != null =>
        Expression.IntLiteral(number.INT().getText.toInt)

      case id if id.ID() != null =>
        Expression.Identifier(id.ID().getText)

      case funcCall if funcCall.functionCall() != null =>
        val ctx = funcCall.functionCall()
        val id: Expression.Identifier = Expression.Identifier(ctx.ID().getText)
        val args = Option(ctx.argList()).toList.flatMap { argList =>
          argList.expr().asScala.map(buildExpression).toList
        }
        Expression.FunctionCall(id, args)

      case parenthesized if parenthesized.getChild(0).getText == "(" =>
        buildExpression(parenthesized.expr())

      case _ =>
        throw new IllegalArgumentException("Unknown primary expression type")
    }
  }
}
