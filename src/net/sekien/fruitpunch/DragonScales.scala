package net.sekien.fruitpunch

import java.io._

import scala.reflect.internal.util.ScalaClassLoader.URLClassLoader
import scala.reflect.io.File
import scala.util.parsing.input.CharSequenceReader

object DragonScales {
  def apply(root: Context): Unit = {
    val proto: Context = root.getv("proto").asInstanceOf[Context]
    val sys: Context = root.getv("sys").asInstanceOf[Context]
    val nativ: Context = root.touchv("native")
    val java = sys.touchv("java")

    def native(context: Context, name: String, fn: (Stack, Type, Evaluator) => Unit): Unit = {
      context.bindv(name, new NativeClosure(name) {
        override def apply(stack: Stack, context: Type, eval: Evaluator): Unit = fn(stack, context, eval)
      })
    }

    def bool(value: Boolean): Type = {if (value) root.getv("true") else root.getv("false")}

    // basic memory functions
    native(root, "getv", (stack: Stack, self: Type, eval: Evaluator) => {
      val key = stack.popAsStr
      val obj = eval.asContext(stack.pop())
      stack.push(obj.getv(key))
    })
    native(root, "setv", (stack: Stack, self: Type, eval: Evaluator) => {
      val value = stack.pop()
      val key = stack.popAsStr
      val obj = stack.popAs[Context]
      obj.setv(key, value)
    })
    native(root, "bindv", (stack: Stack, self: Type, eval: Evaluator) => {
      val value = stack.pop()
      val key = stack.popAsStr
      val obj = stack.popAs[Context]
      obj.bindv(key, value)
    })
    native(root, "hasv", (stack: Stack, self: Type, eval: Evaluator) => {
      val key = stack.popAsStr
      val obj = stack.pop()
      obj match {
        case context: Context =>
          stack.push(bool(context.map.contains(key)))
        case _ => stack.push(bool(false))
      }
    })
    native(root, "remv", (stack: Stack, self: Type, eval: Evaluator) => {
      val key = stack.popAsStr
      val obj = stack.popAs[Context]
      obj.map.remove(key)
    })
    native(root, "lsv", (stack: Stack, self: Type, eval: Evaluator) => {
      val obj = eval.asContext(stack.pop())
      obj.map.keysIterator foreach {k => stack.push(StringValue(k))}
    })
    native(root, "getp", (stack: Stack, self: Type, eval: Evaluator) => {
      val obj = stack.pop()
      obj match {
        case context: Context =>
          val parent: Context = context.parent
          stack.push(if (parent == null) root.getv("nil") else parent)
        case closure: Closure =>
          stack.push(closure.parent)
        case other => stack.push(eval.asContext(obj))
      }
    })
    native(root, "setp", (stack: Stack, self: Type, eval: Evaluator) => {
      val parent = stack.popAs[Context]
      val obj = stack.pop()
      obj match {
        case _: RootContext =>
          throw new FruityException("cannot set root's parent")
        case context: Context => context.parent = parent
        case closure: Closure => closure.parent = parent
        case _ =>
          throw new FruityException("cannot set parent on primitive type " + Type.getTypeName(obj.getClass))
      }
    })

    // combinators
    native(root, "list", (stack: Stack, self: Type, eval: Evaluator) => {
      val list = stack.toList
      stack.clear()
      stack.push(ListValue(list))
    })
    native(root, "map", (stack: Stack, self: Type, eval: Evaluator) => {
      val f = stack.popAs[Closure]
      val l = stack.toList
      stack.clear()
      l foreach {n =>
        stack.push(n)
        eval.executeCallable(f, stack)
      }
    })
    native(root, "tailrec", (stack: Stack, self: Type, eval: Evaluator) => {
      val f = stack.popAs[Closure]
      stack.push(TailRecClosure(f))
    })

    // sys / misc
    native(sys, "intern", (stack: Stack, self: Type, eval: Evaluator) => {
      stack.push(StringValue(PrettyPrinter(stack.pop())))
    })
    native(sys, "exit", (stack: Stack, self: Type, eval: Evaluator) => {
      System.exit(1)
    })
    native(sys, "dump", (stack: Stack, self: Type, eval: Evaluator) => {
      println(stack)
    })
    native(sys, "breakpoint", (stack: Stack, self: Type, eval: Evaluator) => {
      println("<breakpoint>")
    })
    native(root, "stksize", (stack: Stack, self: Type, eval: Evaluator) => {
      stack.push(DoubleValue(stack.length))
    })
    native(root, "reverse", (stack: Stack, self: Type, eval: Evaluator) => {
      val l = stack.toListReversed
      stack.clear()
      l foreach stack.push
    })
    native(root, "clear", (stack: Stack, self: Type, eval: Evaluator) => {
      stack.clear()
    })
    native(root, "dot", (stack: Stack, self: Type, eval: Evaluator) => {
      stack.dot(stack.popAsNum.toInt)
    })
//    native(root, "print", (stack: Stack, self: Type, eval: Evaluator) => {
//      println(stack.popAsStr)
//    })
    native(root, "throw", (stack: Stack, self: Type, eval: Evaluator) => {
      throw new FruityException(stack.popAsStr)
    })
    native(root, "try", (stack: Stack, self: Type, eval: Evaluator) => {
      val handler = stack.popAs[Callable]
      val attempt = stack.popAs[Callable]
      try {
        eval.executeCallable(attempt, stack)
      } catch {
        case e: FruityException => {
          val exobj = new Context(proto.touchv("Exception"))
          exobj.bindv("msg", StringValue(e.message))
          exobj.bindv("longstr", StringValue(e.longString))
          exobj.bindv("shortstr", StringValue(e.shortString))
          stack.push(exobj)
          eval.executeCallable(handler, stack)
        }
      }
    })
    native(sys, "time", (stack: Stack, self: Type, eval: Evaluator) => {
      stack.push(new DoubleValue(System.currentTimeMillis()))
    })
    native(root, "typeof", (stack: Stack, self: Type, eval: Evaluator) => {
      stack.push(StringValue(Type.getTypeName(stack.pop().getClass)))
    })
    native(root, "disasm", (stack: Stack, self: Type, eval: Evaluator) => {
      val closure = stack.popAs[Closure]
      stack.push(ListValue(closure.args map StringValue))
      stack.push(ListValue(closure.code))
    })
    native(root, "asm", (stack: Stack, self: Type, eval: Evaluator) => {
      val code = stack.popAs[ListValue].value
      val args: List[String] = stack.popAs[ListValue].value map {
        case s: StringValue => s.value
        case other => throw new FruityException("asm args list must only contain String, got " + Type.getTypeName(other.getClass))
      }
      stack.push(PushClosure(args, code))
    })
    native(root, "execinstr", (stack: Stack, self: Type, eval: Evaluator) => {
      val _self = stack.pop()
      val context = stack.popAs[Context]
      val instr = stack.popAs[InstructionType]
      eval.executeToken(instr, stack, context, _self)
    })
    native(nativ, "exec", (stack: Stack, self: Type, eval: Evaluator) => {
      val _self = stack.pop()
      val context = stack.popAs[Context]
      val instrs = stack.popAs[ListValue]
      eval.executeList(instrs.value, stack, context, _self)
    })
    native(root, "parse", (stack: Stack, self: Type, eval: Evaluator) => {
      eval.parser.parse(stack.popAsStr) match {
        case eval.parser.Success(result: List[Type], next) => stack.push(ListValue(result))
        case eval.parser.NoSuccess(msg, next) => throw new FruityException(msg + "\n" + next.pos.longString)
      }
    })
    val loadPath = File(System.getenv("FRUITY_HOME")+"/native").toURI.toURL
    val jarsPath = File(System.getenv("FRUITY_HOME")+"/native/lib/lwjgl.jar").toURI.toURL
    val nativecl = new URLClassLoader(List(loadPath, jarsPath), this.getClass.getClassLoader)
    native(nativ, "loadnative", (stack: Stack, self: Type, eval: Evaluator) => {
      val cname = stack.popAsStr
      try {
        nativecl.loadClass(cname).newInstance().asInstanceOf[NativeInterface].apply(root)
      } catch {
        case e: ClassNotFoundException => throw new FruityException("native class not found: " + cname)
      }
    })
    native(nativ, "loadjar", (stack: Stack, self: Type, eval: Evaluator) => {
      val jarname = stack.popAsStr
      try {
        nativecl.addURL(File(loadPath+"/lib/"+jarname).toURI.toURL)
        print(nativecl.classPathURLs)
      } catch {
        case e: Exception => throw new FruityException("failed to load jar " + jarname + "\n" + e.getMessage)
      }
    })

    def highlightCode(ts: List[Type]): String = (ts map {f => PrettyPrinter(f, new HtmlSyntaxHighlighter)}).mkString(" ")

    native(nativ, "syntaxHighlighter", (stack: Stack, self: Type, eval: Evaluator) => {
      stack.push(StringValue(highlightCode(eval.parser.parse(new CharSequenceReader(stack.popAsStr)).getOrElse(List()))))

    })
    native(nativ, "includeIn", (stack: Stack, self: Type, eval: Evaluator) => {
      val fname = stack.popAsStr
      val context = stack.popAs[Context]
      val tokens: List[Type] = eval.parser.parseFile(File(fname))
      eval.executeList(tokens, stack, context, self)
    })
    root.bindv("nop", Nop())

    // multithreading
    native(root, "fork", (stack: Stack, self: Type, eval: Evaluator) => {
      val closure = stack.popAs[Closure]
      val _stack = new Stack(null)
      stack.toList foreach _stack.push
      EvaluatorPool.runTask(_stack, closure, root.asInstanceOf[RootContext])
    })
    native(root, "sleep", (stack: Stack, self: Type, eval: Evaluator) => {
      val time = stack.popAsNum
      Thread.sleep(time.toLong)
    })

    // number
    native(nativ, "add", (stack: Stack, self: Type, eval: Evaluator) => {
      stack.push(DoubleValue(stack.popAsNum + stack.popAsNum))
    })
    native(nativ, "sub", (stack: Stack, self: Type, eval: Evaluator) => {
      stack.push(DoubleValue(stack.popAsNum - stack.popAsNum))
    })
    native(nativ, "mul", (stack: Stack, self: Type, eval: Evaluator) => {
      stack.push(DoubleValue(stack.popAsNum * stack.popAsNum))
    })
    native(nativ, "div", (stack: Stack, self: Type, eval: Evaluator) => {
      stack.push(DoubleValue(stack.popAsNum / stack.popAsNum))
    })
    native(nativ, "mod", (stack: Stack, self: Type, eval: Evaluator) => {
      stack.push(DoubleValue(stack.popAsNum % stack.popAsNum))
    })
    native(nativ, "pow", (stack: Stack, self: Type, eval: Evaluator) => {
      stack.push(DoubleValue(Math.pow(stack.popAsNum, stack.popAsNum)))
    })
    native(nativ, "cmp", (stack: Stack, self: Type, eval: Evaluator) => {
      stack.push(DoubleValue(stack.popAsNum.compareTo(stack.popAsNum)))
    })

    // io
    java.bindv("stdout", JavaWrapper(System.out))
    java.bindv("stdin", JavaWrapper(System.in))
    native(nativ, "write", (stack: Stack, self: Type, eval: Evaluator) => {
      val output: OutputStream = stack.popAs[JavaWrapper].value.asInstanceOf[OutputStream]
      output.write(stack.popAsNum.toInt)
    })
    native(nativ, "read", (stack: Stack, self: Type, eval: Evaluator) => {
      val input: InputStream = stack.popAs[JavaWrapper].value.asInstanceOf[InputStream]
      stack.push(new DoubleValue(input.read()))
    })
    native(nativ, "flush", (stack: Stack, self: Type, eval: Evaluator) => {
      val output: OutputStream = stack.popAs[JavaWrapper].value.asInstanceOf[OutputStream]
      output.flush()
    })
    native(nativ, "close", (stack: Stack, self: Type, eval: Evaluator) => {
      stack.popAs[JavaWrapper].value match {
        case input: InputStream => input.close()
        case output: OutputStream => output.close()
        case other => FruityException("expected native InputStream or OutputStream got " + other.getClass.getSimpleName)
      }
    })

    // string
    native(nativ, "sconcat", (stack: Stack, self: Type, eval: Evaluator) => {
      stack.push(StringValue(stack.popAsStr + stack.popAsStr))
    })
    native(nativ, "strtochar", (stack: Stack, self: Type, eval: Evaluator) => {
      stack.popAsStr foreach {c => stack.push(DoubleValue(c))}
    })
    native(nativ, "chartostr", (stack: Stack, self: Type, eval: Evaluator) => {
      val str = new StringValue((stack.toList map {
        case DoubleValue(n) => n.toChar
        case other => throw new FruityException("expected char got "+Type.getTypeName(other.getClass))}).mkString)
      stack.clear()
      stack.push(str)
    })
    native(nativ, "strtonum", (stack: Stack, self: Type, eval: Evaluator) => {
      stack.push(DoubleValue(stack.popAsStr.toDouble))
    })
    native(nativ, "strsplit", (stack: Stack, self: Type, eval: Evaluator) => {
      stack.popAsStr.split(stack.popAsStr) foreach {t => stack.push(StringValue(t))}
    })
    native(nativ, "strrep", (stack: Stack, self: Type, eval: Evaluator) => {
      val t = stack.popAsStr.replace(stack.popAsStr, stack.popAsStr)
      stack.push(StringValue(t))
    })

    // list
    native(nativ, "stktolist", (stack: Stack, self: Type, eval: Evaluator) => {
      val list: ListValue = ListValue(stack.toList)
      stack.clear()
      stack.push(list)
    })
    native(nativ, "listtostk", (stack: Stack, self: Type, eval: Evaluator) => {
      val list: ListValue = stack.popAs[ListValue]
      list.value foreach stack.push
    })
    native(nativ, "listlen", (stack: Stack, self: Type, eval: Evaluator) => {
      val list: ListValue = stack.popAs[ListValue]
      stack.push(DoubleValue(list.value.length))
    })
    native(nativ, "listupdate", (stack: Stack, self: Type, eval: Evaluator) => {
      val list: ListValue = stack.popAs[ListValue]
      list.value = stack.toList
      stack.clear()
    })

    // java interop
//    val rm = scala.reflect.runtime.currentMirror
//    def wrapNative(t: Any): Type = t match {
//      case n: Number => DoubleValue(n.doubleValue())
//      case s: String => StringValue(s)
//      case b: Boolean => bool(b)
//      case l: List[Any] => ListValue(l map {e => wrapNative(e)})
//      case other => JavaWrapper(other)
//    }
//    def unwrapNative(stack: Stack): List[Any] = {
//      val l = stack.toList map {
//        case n: DoubleValue => n.value
//        case s: StringValue => s.value
//        case j: JavaWrapper => j.value
//        case other => throw new FruityException("cannot unwrap type " + Type.getTypeName(other.getClass()))
//      }
//      stack.clear()
//      l
//    }
//    native(nativ, "javaGetField", (stack: Stack, self: Type, eval: Evaluator) => {
//      val name = stack.popAsStr
//      val obj = stack.popAs[JavaWrapper]
//      val im = rm.reflect(obj.value)
//      stack.push(wrapNative(im.reflectField(rm.classSymbol(obj.value.getClass).toType.decl(TermName(name)).asTerm).get))
//    })
//    native(nativ, "javaCallMethod", (stack: Stack, self: Type, eval: Evaluator) => {
//      val name = stack.popAsStr
//      val obj = stack.popAs[JavaWrapper]
//      val im = rm.reflect(obj.value)
//      stack.push(wrapNative(im.reflectMethod(rm.classSymbol(obj.value.getClass).toType.decl(TermName(name)).asMethod).apply(unwrapNative(stack))))
//    })

    // logic
    native(root, "is", (stack: Stack, self: Type, eval: Evaluator) => {
      stack.push(bool(stack.pop().equals(stack.pop())))
    })
  }
}
