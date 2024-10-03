package compiler

import parser.{BinaryOperator, Expression, FunctionDeclaration, Program, Statement}

object Compiler {
  type MachineCodeChunk = Vector[Short]
  type ConstantPool = Vector[Int]
  case class FunctionMetadata(name: String, arity: Int)
  case class CompiledProgram(
      chunks: Vector[MachineCodeChunk],
      functionsMetadata: Vector[FunctionMetadata],
      constantPool: ConstantPool,
      mainFunctionIndex: Int
  )

  private type VariablesStack = Vector[String]
  private case class CompilerContext(
      functionsMetadata: Vector[FunctionMetadata]
  )

  def compileProgram(program: Program): CompiledProgram = {
    if (program.functions.view.map(_.id.name).distinct.size != program.functions.size)
      throw IllegalArgumentException("Functions must have unique names")

    val functionsMetadata = program.functions.map(_.metadata).toVector
    given CompilerContext = CompilerContext(functionsMetadata)

    val mainFunctionIndex = program.functions.indexWhere(_.id.name == MainFunctionName)
    if (mainFunctionIndex == -1) throw IllegalArgumentException("Main function not found")

    val (chunks, newConstantPool) = program.functions.foldLeft(
      (Vector.empty[MachineCodeChunk], Vector.empty[Int])
    ) { case ((chunks, constantPool), function) =>
      val (chunk, newConstantPool) = compileFunction(function, constantPool)
      (chunks :+ chunk, newConstantPool)
    }

    CompiledProgram(chunks, functionsMetadata, newConstantPool, mainFunctionIndex)
  }

  private def compileFunction(
      function: FunctionDeclaration,
      constantPool: ConstantPool
  )(using CompilerContext): (MachineCodeChunk, ConstantPool) = {
    val (chunk, newConstantPool) =
      compileBlock(function.body, constantPool, function.params.map(_.name).toVector)
    val (returnZeroChunk, newConstantPool2, _) = compileStatement(
      Statement.ReturnStatement(Expression.IntLiteral(0)),
      newConstantPool,
      Vector.empty[String]
    )
    (chunk ++ returnZeroChunk, newConstantPool2)
  }

  private def compileBlock(
      block: Statement.Block,
      constantPool: ConstantPool,
      variablesStack: VariablesStack
  )(using CompilerContext): (MachineCodeChunk, ConstantPool) = {
    val (chunks, newConstantPool, newVariablesStack) =
      block.statements.foldLeft((Vector.empty[Short], constantPool, variablesStack)) {
        case ((chunks, constantPool, variablesStack), statement) =>
          val (chunk, newConstantPool, newVariablesStack) = compileStatement(
            statement,
            constantPool,
            variablesStack
          )
          (chunks ++ chunk, newConstantPool, newVariablesStack)
      }
    (
      chunks ++ Vector.fill(newVariablesStack.size - variablesStack.size)(Instruction.Pop.opCode),
      newConstantPool
    )
  }

  private def compileStatement(
      statement: Statement,
      constantPool: ConstantPool,
      variablesStack: VariablesStack
  )(using CompilerContext): (MachineCodeChunk, ConstantPool, VariablesStack) = statement match {
    case block: Statement.Block =>
      val (compiledBlock, newConstantPool) = compileBlock(block, constantPool, variablesStack)
      (compiledBlock, newConstantPool, variablesStack)

    case Statement.LetStatement(id, expr) =>
      val (compiledExpr, newConstantPool) = compileExpression(expr, constantPool, variablesStack)
      (compiledExpr, newConstantPool, variablesStack :+ id.name)

    case Statement.AssignmentStatement(id, expr) =>
      val (compiledExpr, newConstantPool) = compileExpression(expr, constantPool, variablesStack)
      val variableIndex = variablesStack.lastIndexOf(id.name)
      if (variableIndex == -1)
        throw IllegalArgumentException(s"Variable ${id.name} not found in scope")
      (
        compiledExpr ++ Vector(Instruction.SetLocal.opCode, variableIndex.toShort),
        newConstantPool,
        variablesStack
      )

    case Statement.IfStatement(condition, thenBlock, elseBlock) =>
      val (compiledCondition, newConstantPool) = compileExpression(
        condition,
        constantPool,
        variablesStack
      )
      val (compiledThenBlock, newConstantPool2) = compileBlock(
        thenBlock,
        newConstantPool,
        variablesStack
      )
      val (compiledElseBlock, newConstantPool3) = elseBlock match {
        case Some(elseBlock) => compileBlock(elseBlock, newConstantPool2, variablesStack)
        case None            => (Vector.empty[Short], newConstantPool2)
      }
      (
        compiledCondition
          ++ Vector(Instruction.JumpIfFalse.opCode, compiledThenBlock.length.toShort)
          ++ compiledThenBlock
          ++ Vector(Instruction.Jump.opCode, compiledElseBlock.length.toShort)
          ++ compiledElseBlock,
        newConstantPool3,
        variablesStack
      )

    case Statement.WhileStatement(condition, body) =>
      val (compiledCondition, newConstantPool) = compileExpression(
        condition,
        constantPool,
        variablesStack
      )
      val (compiledBody, newConstantPool2) = compileBlock(
        body,
        newConstantPool,
        variablesStack
      )
      (
        compiledCondition
          ++ Vector(Instruction.JumpIfFalse.opCode, (compiledBody.length + 2).toShort)
          ++ compiledBody
          ++ Vector(
            Instruction.Jump.opCode,
            (-compiledBody.length - 4 - compiledCondition.length).toShort
          ),
        newConstantPool2,
        variablesStack
      )

    case Statement.ReturnStatement(expr) =>
      val (compiledExpr, newConstantPool) = compileExpression(expr, constantPool, variablesStack)
      (compiledExpr ++ Vector(Instruction.Return.opCode), newConstantPool, variablesStack)
  }

  private def compileExpression(
      expr: Expression,
      constantPool: ConstantPool,
      variablesStack: VariablesStack
  )(using ctx: CompilerContext): (MachineCodeChunk, ConstantPool) = expr match {
    case Expression.IntLiteral(value) =>
      val constantIndex = constantPool.indexWhere(_ == value)
      if (constantIndex == -1)
        (Vector(Instruction.Constant.opCode, constantPool.size.toShort), constantPool :+ value)
      else
        (Vector(Instruction.Constant.opCode, constantIndex.toShort), constantPool)

    case Expression.Identifier(name) =>
      val index = variablesStack.lastIndexOf(name)
      if (index == -1) throw IllegalArgumentException(s"Variable $name not found in scope")
      (Vector(Instruction.GetLocal.opCode, index.toShort), constantPool)

    case Expression.BinaryOperation(left, op, right) =>
      val (compiledLeft, newConstantPool) = compileExpression(
        left,
        constantPool,
        variablesStack
      )
      val (compiledRight, newConstantPool2) = compileExpression(
        right,
        newConstantPool,
        variablesStack
      )
      val opCode = op match {
        case BinaryOperator.Add                => Instruction.Add.opCode
        case BinaryOperator.Subtract           => Instruction.Subtract.opCode
        case BinaryOperator.Multiply           => Instruction.Multiply.opCode
        case BinaryOperator.Divide             => Instruction.Divide.opCode
        case BinaryOperator.Equal              => Instruction.Equal.opCode
        case BinaryOperator.NotEqual           => Instruction.NotEqual.opCode
        case BinaryOperator.GreaterThan        => Instruction.GreaterThan.opCode
        case BinaryOperator.LessThan           => Instruction.LessThan.opCode
        case BinaryOperator.GreaterThanOrEqual => Instruction.GreaterThanOrEqual.opCode
        case BinaryOperator.LessThanOrEqual    => Instruction.LessThanOrEqual.opCode
      }
      (compiledLeft ++ compiledRight ++ Vector(opCode), newConstantPool2)

    case Expression.FunctionCall(id, args) =>
      val (compiledArgs, newConstantPool) = args.foldLeft((Vector.empty[Short], constantPool)) {
        case ((chunks, constantPool), arg) =>
          val (chunk, newConstantPool) = compileExpression(
            arg,
            constantPool,
            variablesStack
          )
          (chunks ++ chunk, newConstantPool)
      }

      val functionIndex = ctx.functionsMetadata.indexWhere(_.name == id.name)
      if (functionIndex != -1) {
        val arity = ctx.functionsMetadata(functionIndex).arity
        if (arity != args.size)
          throw IllegalArgumentException(
            s"Function ${id.name} expects $arity arguments, but got ${args.size}"
          )
        (compiledArgs ++ Vector(Instruction.Call.opCode, functionIndex.toShort), newConstantPool)
      } else {
        val builtinFunction = BuiltinFunction
          .fromName(id.name)
          .getOrElse(throw IllegalArgumentException(s"Function $id not found in scope"))
        val builtinFunctionIndex = builtinFunction.index
        val arity = builtinFunction.arity
        if (arity != args.size)
          throw IllegalArgumentException(
            s"Function ${id.name} expects $arity arguments, but got ${args.size}"
          )
        (
          compiledArgs ++ Vector(Instruction.CallBuiltin.opCode, builtinFunctionIndex.toShort),
          newConstantPool
        )
      }
  }

  private val MainFunctionName = "main"
  extension (f: FunctionDeclaration) {
    private def metadata = FunctionMetadata(f.id.name, f.params.size)
  }
}
