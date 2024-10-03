package vm

import compiler.Compiler.CompiledProgram
import compiler.{BuiltinFunction, Compiler, Instruction}

import java.io.PrintStream
import scala.collection.mutable

class VM(outputStream: PrintStream) {
  private class CallFrame(
      val functionIndex: Int,
      var instructionPointer: Int,
      val framePointer: Int
  )

  private val stack = mutable.ArrayBuffer.empty[Int]
  private val callStack = mutable.ArrayBuffer.empty[CallFrame]

  def run(compiledProgram: CompiledProgram): Unit = {
    val mainFunctionIndex = compiledProgram.mainFunctionIndex
    callStack.append(CallFrame(mainFunctionIndex, 0, 0))

    while (true) {
      while (
        callStack.nonEmpty &&
        callStack.top.instructionPointer >=
          compiledProgram.chunks(callStack.top.functionIndex).length
      ) callStack.pop()
      if (callStack.isEmpty) return

      val callFrame = callStack.top
      val chunk = compiledProgram.chunks(callFrame.functionIndex)
      val opCode = chunk(callFrame.instructionPointer)

      val instruction = Instruction
        .fromOpCode(opCode)
        .getOrElse(throw IllegalArgumentException(s"Unknown instruction $opCode"))

      instruction match {
        case Instruction.Constant => {
          val constantIndex = chunk(callFrame.instructionPointer + 1)
          stack.append(compiledProgram.constantPool(constantIndex))
          callFrame.instructionPointer += 2
        }

        case Instruction.Pop => {
          stack.pop()
          callFrame.instructionPointer += 1
        }

        case Instruction.GetLocal => {
          val localIndex = chunk(callFrame.instructionPointer + 1)
          stack.append(stack(callFrame.framePointer + localIndex))
          callFrame.instructionPointer += 2
        }

        case Instruction.SetLocal => {
          val localIndex = chunk(callFrame.instructionPointer + 1)
          stack(callFrame.framePointer + localIndex) = stack.pop()
          callFrame.instructionPointer += 2
        }

        case Instruction.Add | Instruction.Subtract | Instruction.Multiply | Instruction.Divide |
            Instruction.Equal | Instruction.NotEqual | Instruction.GreaterThan |
            Instruction.LessThan | Instruction.GreaterThanOrEqual | Instruction.LessThanOrEqual => {
          val right = stack.pop()
          val left = stack.pop()
          stack.append(computeBinaryOperation(instruction, left, right))
          callFrame.instructionPointer += 1
        }

        case Instruction.Call => {
          val functionIndex = chunk(callFrame.instructionPointer + 1)
          val functionMetadata = compiledProgram.functionsMetadata(
            functionIndex
          )
          val arity = functionMetadata.arity
          callStack.append(CallFrame(functionIndex, 0, stack.size - arity))
          callFrame.instructionPointer += 2
        }

        case Instruction.CallBuiltin => {
          val builtinIndex = chunk(callFrame.instructionPointer + 1)
          val builtinFunction = BuiltinFunction
            .fromIndex(builtinIndex)
            .getOrElse(throw IllegalArgumentException(s"Unknown builtin function $builtinIndex"))
          builtinFunction match {
            case BuiltinFunction.Print => {
              val value = stack.pop()
              outputStream.println(value)
              stack.append(0)
            }
          }
          callFrame.instructionPointer += 2
        }

        case Instruction.Jump => {
          val jumpOffset = chunk(callFrame.instructionPointer + 1)
          callFrame.instructionPointer += 2 + jumpOffset
        }

        case Instruction.JumpIfFalse => {
          val jumpOffset = chunk(callFrame.instructionPointer + 1)
          val value = stack.pop()
          if (value == 0) {
            callFrame.instructionPointer += 2 + jumpOffset
          } else {
            callFrame.instructionPointer += 2
          }
        }

        case Instruction.Return => {
          val returnValue = stack.pop()
          while (stack.size > callFrame.framePointer) stack.pop()
          callStack.pop()
          stack.append(returnValue)
        }
      }
    }
  }

  private def computeBinaryOperation(instruction: Instruction, left: Int, right: Int): Int =
    instruction match {
      case Instruction.Add                => left + right
      case Instruction.Subtract           => left - right
      case Instruction.Multiply           => left * right
      case Instruction.Divide             => left / right
      case Instruction.Equal              => if (left == right) 1 else 0
      case Instruction.NotEqual           => if (left != right) 1 else 0
      case Instruction.GreaterThan        => if (left > right) 1 else 0
      case Instruction.LessThan           => if (left < right) 1 else 0
      case Instruction.GreaterThanOrEqual => if (left >= right) 1 else 0
      case Instruction.LessThanOrEqual    => if (left <= right) 1 else 0
      case _ => throw IllegalArgumentException(s"$instruction is not a binary operator")
    }

  extension [A](a: mutable.ArrayBuffer[A]) {
    private def pop(): A = a.remove(a.length - 1)
    private def top: A = a(a.length - 1)
  }
}
