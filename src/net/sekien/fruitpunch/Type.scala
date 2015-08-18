package net.sekien.fruitpunch

import scala.collection.mutable
import scala.reflect.io.File
import scala.util.parsing.input.Positional

// Classes to represent each data type / instruction
class Type extends Positional
trait Callable

// Value types push themselves to the stack
sealed class ValueType extends Type
case class Closure(args: List[String], code: List[Type], var parent: Context) extends ValueType with Callable
case class TailRecClosure(closure: Closure) extends ValueType with Callable
case class IntegerValue(value: Int) extends ValueType
case class DoubleValue(value: Double) extends ValueType
case class StringValue(value: String) extends ValueType
case class SymbolValue(value: String) extends ValueType
case class ArgumentValue(value: String) extends ValueType
case class ListValue(var value: List[Type]) extends ValueType
case class JavaWrapper(value: Any) extends ValueType
case class Nop() extends ValueType with Callable

// Instruction type perform a specific action involving memory
sealed class InstructionType extends Type
case class CallVar(value: String) extends InstructionType
case class InfixVar(value: String, arg: Type) extends InstructionType
case class InfixIncludeVar(arg: String, file: File) extends InstructionType
case class GetVar(value: String) extends InstructionType
case class SetVar(value: String) extends InstructionType
case class BindVar(value: String) extends InstructionType
case class PreBindVar(value: String, arg: Type) extends InstructionType
case class CallList(value: String, code: List[Type]) extends InstructionType
case class OpenList(code: List[Type]) extends InstructionType
case class ClosedList(code: List[Type]) extends InstructionType
case class PushClosure(args: List[String], code: List[Type]) extends InstructionType
case class ObjectVar(closure: Closure) extends InstructionType
case class SelfRef(isContext: Boolean) extends InstructionType
case class IfThenElse(t: Type, f: Option[Type]) extends InstructionType
case class DotOperator(n: Int) extends InstructionType

abstract class NativeClosure(val fname: String) extends ValueType with Callable {
  def apply(stack: Stack, context: Type, eval: Evaluator)
}

object Type {
  def getTypeName(t: Class[_]) = t.getSimpleName match {
    case "Closure" => "Closure"
    case "Context" => "Object"
    case "DoubleValue" => "Number"
    case "StringValue" => "String"
    case "SymbolValue" => "Symbol"
    case "ArgumentValue" => "Argument"
    case "ListValue" => "List"
    case "InstructionType" => "Instruction"
    case "NativeClosure" => "NativeClosure"
    case other => "Java"+other
  }
  val k: DoubleValue = DoubleValue(5)
}
