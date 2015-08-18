package net.sekien.fruitpunch

import java.math.RoundingMode
import java.text.DecimalFormat

object PrettyPrinter {
  val decFormat = new DecimalFormat("#.###")
  decFormat.setRoundingMode(RoundingMode.FLOOR)
  
  def apply(l: List[Type]): String = (l map apply).mkString(" ")
  def apply(t: Type): String = t match {
    case PushClosure(args, code) => "{" +
      (if (args.nonEmpty) {
        args.mkString(" ") + "=>"
      } else {
        ""
      }) +
      apply(code) + "}"
    case Closure(args, code, _) => apply(PushClosure(args, code))
    case IntegerValue(value) => value.toString
    case DoubleValue(value) => decFormat.format(value)
    case StringValue(value) => s"'$value'" //todo ensure this escapes stuff
    case SymbolValue(value) => s"#$value"
    case ArgumentValue(value) => s"@$value"

    case CallVar(value) => value
    case InfixVar(value, arg) => value + " " + apply(arg)
    case InfixIncludeVar(arg, file) => "include " + arg
    case GetVar(value) => "$"+value
    case SetVar(value) => ">"+value
    case BindVar(value) => ">>"+value
    case PreBindVar(value, arg) => value + ": " + apply(arg)
    case CallList(value, code) => s"$value(${apply(code)})"
    case OpenList(code) => "(" + apply(code) + ")"
    case ClosedList(code) => "[" + apply(code) + "]"
    case ObjectVar(closure) => ":" + apply(closure)
    case IfThenElse(t, f) => "then " + apply(t) + (f match {
      case Some(f: Type) => " else " + apply(f)
      case None => ""
    })
    case DotOperator(n) => "." * n
    case native: NativeClosure => "<"+native.fname+">"
    case _: Nop => "<the elusive nop in its natural habitat>"
    case other => "<java "+other.getClass.getSimpleName+"@"+System.identityHashCode(other)+">"
  }

  def apply(l: List[Type], sh: SyntaxHighlighter): String = (l map {apply(_, sh)}).mkString(" ")
  def apply(t: Type, sh: SyntaxHighlighter): String = t match {
    case PushClosure(args, code) => "{" +
      (if (args.nonEmpty) {
        sh.variable(args.mkString(" ")) + " " + sh.infix("=>") + " "
      } else {
        ""
      }) +
      apply(code, sh) + "}"
    case Closure(args, code, _) => apply(PushClosure(args, code), sh)
    case IntegerValue(value) => sh.num(value.toString)
    case DoubleValue(value) => sh.num(decFormat.format(value))
    case StringValue(value) => sh.str(s"'$value'") //todo ensure this escapes stuff
    case SymbolValue(value) => sh.sym(s"#$value")
    case ArgumentValue(value) => sh.arg(s"@$value")

    case CallVar(value) => if (value == "true" || value == "false") sh.num(value) else sh.call(value)
    case InfixVar(value, arg) => sh.infix(InfixTable.reverseLookup(value)) + " " + apply(arg, sh)
    case InfixIncludeVar(arg, file) => sh.infix("include") + " " + sh.str(arg)
    case GetVar(value) => sh.variable("$"+value)
    case SetVar(value) => sh.variable(">"+value)
    case BindVar(value) => sh.variable(">>"+value)
    case PreBindVar(value, arg) => sh.variable(value + ":") + " " + apply(arg, sh)
    case CallList(value, code) => sh.call(value) + "(" + apply(code, sh) + ")"
    case OpenList(code) => "(" + apply(code, sh) + ")"
    case ClosedList(code) => "[" + apply(code, sh) + "]"
    case ObjectVar(closure) => ":" + apply(closure, sh)
    case IfThenElse(t, f) => sh.infix("then")+" " + apply(t, sh) + (f match {
      case Some(f: Type) => " "+sh.infix("else")+" " + apply(f, sh)
      case None => ""
    })
    case DotOperator(n) => sh.infix("." * n)
    case native: NativeClosure => "<"+native.fname+">"
    case _: Nop => "<the elusive nop in its natural habitat>"
    case other => "<java "+other.getClass.getSimpleName+"@"+System.identityHashCode(other)+">"
  }
}
