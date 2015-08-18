package net.sekien.fruitpunch

import scala.reflect.io.File

class Evaluator(val DEBUG: Boolean, val parser: Parser, val _root: RootContext) {
  def evalString(code: String): Stack = {
    val stack = new Stack(null)
    parser.parse(code) match {
      case parser.Success(result: List[Type], next) =>
        executeList(result, stack, root, root)
        stack
      case parser.NoSuccess(msg, next) => throw new FruityException(s"failed to parse:\n$msg\n${next.pos.longString}")
    }
  }

  val (root: RootContext, proto: Context, sys: Context) = if (_root == null) {
    val root = new RootContext("root")
    val proto = new Context(root)
    val sys = new Context(root)
    root.bindv("proto", proto)
    root.bindv("sys", sys)
    proto.bindv("All", new RootContext("all"))
    (root, proto, sys)
  } else {
    (_root, _root.getv("proto"), _root.getv("sys"))
  }

  def asContext(value: Type): Context = (value match {
    case _: Context => value
    case _: Closure => proto.getv("Closure")
    case _: DoubleValue => proto.getv("Number")
    case _: StringValue => proto.getv("String")
    case _: SymbolValue => proto.getv("Symbol")
    case _: ArgumentValue => proto.getv("Argument")
    case _: ListValue => proto.getv("List")
    case _: JavaWrapper => proto.getv("JavaWrapper")
    case _: InstructionType => proto.getv("Instruction")
  }).asInstanceOf[Context]

  def resolve(key: String, stack: Stack, context: Context): (Type, String) = {
    var (cur: Type, rem) = key.charAt(0) match {
      case '.' => (stack.pop(), key.substring(1))
      case ':' => (stack.peek(), key.substring(1))
      case _ => (context, key)
    }
    var remdi = rem.indexOf('.')
    while (remdi != -1) {
      cur = asContext(cur).getv(rem.substring(0,remdi))
      rem = rem.substring(remdi+1)
      remdi = rem.indexOf('.')
    }
    (cur, rem)
  }

  def executeCallable(c: Callable, stack: Stack, self: Option[Type] = None): Unit = {
    c match {
      case closure: Closure =>
//        val context = new Context(if (closure.parent != null) closure.parent else asContext(self))
        val context = new Context(closure.parent)
        self match {
          case Some(value) => context.bindv("self", value)
          case None =>
        }
        executeClosure(closure, stack, null, context)
      case native: NativeClosure =>
        native(stack, null, this)
      case tr: TailRecClosure =>
        var cc: Type = tr
        do {
          executeCallable(cc.asInstanceOf[TailRecClosure].closure, stack, self)
          cc = stack.pop()
        } while (cc.isInstanceOf[TailRecClosure])
      case Nop() =>
    }
  }

  def executeClosure(closure: Closure, stack: Stack, self: Type, context: Context): Unit = {
    closure.args.reverseIterator foreach { arg =>
      context.bindv(arg, stack.pop())
    }
    executeList(closure.code, stack, context, self)
  }

  def executeList(t: List[Type], stack: Stack, context: Context, self: Type): Unit = {
    t foreach {f => executeToken(f, stack, context, self)}
  }

  def executeToken(t: Type, stack: Stack, context: Context, self: Type): Unit = try {
    if (DEBUG) {
      println(stack)
      println("context:"+context)
      println("self:"+self)
      println(t.pos.longString)
      println()
    }
    t match {
      case n: PushClosure =>
//        n.parent = context /// i bet it is this
        stack.push(new Closure(n.args,n.code,context))
      case value: ValueType => stack.push(value)
      case CallVar(key) =>
        val (c, k) = resolve(key, stack, context)
        asContext(c).getv(k) match {
          case v: Callable =>
            if (c.eq(context))
              executeCallable(v, stack, None)
            else
              executeCallable(v, stack, Some(c))
          case n =>
            stack.push(n)
        }
      case GetVar(key) =>
        val (c, k) = resolve(key, stack, context)
        stack.push(asContext(c).getv(k))
      case SetVar(key) =>
        val (c, k) = resolve(key, stack, context)
        asContext(c).setv(k, stack.pop())
      case BindVar(key) =>
        val (c, k) = resolve(key, stack, context)
        asContext(c).bindv(k, stack.pop())
      case PreBindVar(key, code) =>
        val (c, k) = resolve(key, stack, context)
        executeToken(code, stack, context, c)
        asContext(c).bindv(k, stack.pop())
      case InfixVar(value, arg) =>
        executeToken(arg, stack, context, self)
        root.getv(value) match {
          case v: Callable =>
            executeCallable(v, stack)
          case n =>
            throw new FruityException("closure expected")
        }
      case CallList(key, code) =>
        val (c, k) = resolve(key, stack, context)
        val newStack = new Stack(stack)
        executeList(code, newStack, context, self)
        asContext(c).getv(k) match {
          case v: Callable =>
            executeCallable(v, newStack, Some(c))
          case n =>
            throw new FruityException("closure expected")
        }
        newStack.merge()
      case OpenList(code) =>
        val newStack = new Stack(stack)
        executeList(code, newStack, context, self)
        newStack.merge()
      case ClosedList(code) =>
        val newStack = new Stack(stack)
        executeList(code, newStack, context, self)
        stack.push(ListValue(newStack.toList))
      case ObjectVar(closure) =>
        val newContext = new Context(context)
//        closure.parent = context
        executeClosure(closure, stack, newContext, newContext)
        stack.push(newContext)
      case SelfRef(n) => n match {
        case true => stack.push(context)
        case false => stack.push(self)
      }
      case IfThenElse(ct, cf) =>
        val pred = asContext(stack.pop())
        executeToken(ct, stack, context, self)
        executeToken(cf getOrElse Nop(), stack, context, self)
        executeCallable(pred.getv("cond").asInstanceOf[Closure], stack)
      case DotOperator(n) =>
        stack.dot(n)
      case InfixIncludeVar(_arg, file) =>
        val arg = if (_arg.endsWith(".fj")) _arg else _arg+".fj"
        if (file != null && (file.parent/arg).exists) {
          executeList(parser.parseFile((file.parent/arg).toFile), stack, context, self)
        } else {
          if (File(arg).exists) {
            executeList(parser.parseFile(File(arg)), stack, context, self)
          } else {
            println(s"file not found $arg")
          }
        }
    }
  } catch {
    case e: FruityException =>
      e.addPos(t.pos)
      throw e
    case e: Throwable =>
      if (FruitPunch.cfig.verbose) e.printStackTrace()
      val fe = new FruityException("caught java throwable:\n  "+e.getMessage)
      fe.addPos(t.pos)
      throw fe
  }
}
