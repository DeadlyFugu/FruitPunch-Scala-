import net.sekien.fruitpunch
import net.sekien.fruitpunch._

import scala.reflect.io.File
import scala.util.parsing.combinator.RegexParsers
import scala.util.parsing.input.CharSequenceReader

class FruityDoc extends RegexParsers {
  val parser = new fruitpunch.Parser
  val runtime = new Evaluator(false, parser, null)
  FruitPunch.initRuntime(parser, runtime, "/Users/matthew.turner/Documents/fruity")
  def readFile(fname: String): String = File(fname).slurp()
  def writeFile(fname: String, text: String): Unit = File(fname).writeAll(text)

  def parse(text: String): String = parse(new CharSequenceReader(text)) match {
    case Success(result, next) => result.mkString
    case e: NoSuccess => throw new FruityException(e.msg)
  }

  def parse: Parser[List[String]] = phrase(rep(tag))
  def tag: Parser[String] = h|p|bite|repl|code|script
  def simpleText = """[^\]]+""".r
  def complexText: Parser[String] = rep1(simpleText|tag) ^^ {ls => ls.mkString}
  def h = "[h " ~> simpleText <~ "]" ^^ {text => s"<h3>$text</h3>"}
  def p = "[p " ~> simpleText <~ "]" ^^ {text => s"<p>$text</p>"}
  def bite = "[bite " ~> "[0-9]+".r ~ simpleText ~ complexText <~ "]" ^^ {case number ~ text ~ rest => "<div id=\""+text.trim.replace(' ', '-')+"\" class=\"bite\"><h2>"+text+"</h2>"+rest+"</div>"}
  def code = "[code" ~> simpleText <~ "]" ^^ highlightString
  def script = "[script" ~> simpleText <~ "]" ^^ highlightString
  def repl: Parser[String] = "[repl" ~> simpleText <~ "]" ^^ {code =>
    (code.split("\n") map {ln =>
      "<span class=\"comment\">&gt;</span> " + highlightString(ln) + "\n" +
      highlightString(runtime.evalString(ln).toList.mkString(" "))
    }).mkString("\n")
  }

  def highlightString(code: String) = highlightCode(parser.parse(new CharSequenceReader(code)).getOrElse(List()))
  def highlightCode(ts: List[Type]): String = (ts map {f => PrettyPrinter(f, new HtmlSyntaxHighlighter)}).mkString(" ")
}

object FruityDoc {
  def convertFile(infile: String, outfile: String): Unit = {
    val fdoc = new FruityDoc
    val in = fdoc.readFile(infile)
    val out = fdoc.parse(in)
    fdoc.writeFile(outfile, out)
  }
}