package net.sekien.fruitpunch

import java.util.concurrent.{Executors, ExecutorService}

import scala.io.StdIn
import scala.reflect.io.File

object FruitPunch {
  val parser = new Parser
  val eval = new Evaluator(false, parser, null)
  var cfig: Config = Config()

  case class Config(file: Option[File] = None, verbose: Boolean = false, fallback: Boolean = false)

  val argumentHandler = new scopt.OptionParser[Config]("scopt") {
    head("FruitPunch", "1.0", "(scala)")
    opt[String]('f',"file") action { (s, c) =>
      c.copy(file = Some(File(s)))
    }
    opt[Unit]('v',"verbose") action { (_, c) =>
      c.copy(verbose = true)
    } text "print detailed information during load"
    opt[Unit]("fallback") abbr "fb" action { (_, c) =>
      c.copy(fallback = true)
    } text "use fallback repl"
  }

  def initRuntime(parser: Parser, eval: Evaluator, FRUITY_HOME: String = System.getenv("FRUITY_HOME")) = {
    val VERBOSE = cfig.verbose
    if (FRUITY_HOME == null) {
      println("FRUITY_HOME not defined")
    } else {
      eval.root.touchv("native").bindv("fruity_home", StringValue(FRUITY_HOME))
      val fruityHome: File = File(FRUITY_HOME)
      if (VERBOSE) println("loading dragonscales")
      DragonScales(eval.root)
      val bootFile = File(fruityHome / "boot.fj")
      if (bootFile.exists) {
        val stack: Stack = new Stack(null)
        try {
          if (VERBOSE) println("parsing boot.fj")
          val parsed: List[Type] = parser.parseFile(bootFile)
          if (VERBOSE) println("executing boot.fj")
          eval.executeList(parsed, stack, eval.root, eval.root)
        } catch {
          case e: FruityException => println("failure in boot\n  " + e.message)
        }
        if (stack.length > 0) {
          println("garbage was left on stack by boot")
        }
      } else {
        println("FRUITY_HOME not valid (could not find boot.fj)")
      }
      loadModules(eval, fruityHome)
      if (VERBOSE) println("modules: " + eval.root.lazies.keys.mkString(" "))
    }
  }

  def loadModules(eval: Evaluator, fruityHome: File): Unit = {
    val moduleDir = fruityHome / "modules"
    moduleDir.jfile.listFiles foreach { d =>
      if (d.isDirectory) {
        eval.root.addLazy(d.getName, {() =>
          eval.evalString("'" + d.toString + "' loadModule")
        })
      }
    }
  }

  /**
   * Main function, initializes the environment
   * @param args Command-line arguments to the application
   */
  def main(args: Array[String]): Unit = {
    // TODO: handle arguments
    cfig = argumentHandler.parse(args, cfig).getOrElse(cfig)
    initRuntime(parser, eval)
    if (!cfig.fallback) try {
      eval.executeToken(CallVar("repl.run"), new Stack(null), eval.root, eval.root)
    } catch {
      case e: FruityException => {
        println("failed to execute repl:")
        println("  " + (if (cfig.verbose) e.longString else e.shortString))
        println("using fallback repl instead")
        repl()
      }
      case e: Exception => {
        if (cfig.verbose) e.printStackTrace()
        println("failed to execute repl:")
        println("  " + e.getMessage)
        println("using fallback repl instead")
        repl()
      }
    } else {
      repl()
    }
  }

  /**
   * Runs the REPL
   */
  def repl(): Unit = {
    println("FruitPunch 1.0 (scala) [fallback]")
    while (true) {
      var input = prompt("> ")
      var parsed: parser.ParseResult[List[Type]] = parser.parse(input)
      while (parsed.isInstanceOf[parser.NoSuccess] && parsed.asInstanceOf[parser.NoSuccess].msg.endsWith("expected but end of source found")) {
        input += ("\n" + prompt(".."))
        parsed = parser.parse(input)
      }
      parsed match {
        case parser.Success(result: List[Type], _) =>
          try {
            val stack = new Stack(null)
            eval.executeList(result, stack, eval.root, eval.root)
            println(stack)
          } catch {
            case e: FruityException =>
              println(e.longString)
          }
        case parser.NoSuccess(msg, in) =>
          println(s"failed: $msg")
      }
    }
  }

  /**
   * Gets input from console.
   * @return String entered by the user
   */
  def prompt(p: String): String = {
    val ln = StdIn.readLine(p)
    ln
  }
}
