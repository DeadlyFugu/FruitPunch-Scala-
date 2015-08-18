package net.sekien.fruitpunch

import scala.reflect.io.File
import scala.util.parsing.combinator.JavaTokenParsers
import scala.util.parsing.input.CharSequenceReader

class Parser extends JavaTokenParsers {
  def parse(input: String): ParseResult[List[Type]] = {
    file = null
    parse(new CharSequenceReader(input))
  }
  def parse: Parser[List[Type]] = phrase(rep(token))
  def parseFile(file: File): List[Type] = {
    this.file = file
    parse(new CharSequenceReader(file.slurp())) match {
    case Success(result, next) => result
    case n: NoSuccess => throw FruityException(s"could not parse $file\n${n.msg}")
  }}
  private val simpleIdent: Parser[String] = """[_a-zA-Z0-9]+""".r
  private def closurePattern(opening: String = "{") = opening ~> opt(rep(simpleIdent) <~ "=>") ~ rep(token) <~ "}"
  private var file: File = null
  protected override val whiteSpace = """(\s|//.*|(?m)/\*(\*(?!/)|[^*])*\*/)+""".r
  private def resolveOp(k: String): String = InfixTable.infixOps.getOrElse(k, k)

  def unescape(s: String) = s.replace("\\\\","\0").replace("\\n","\n").replace("\\t","\t")
    .replace("\\\"","\"").replace("\\'","'").replace("\\\"","\"").replace("\0","\\")

  def token: Parser[Type] = positioned(value|instruction)
  def value: Parser[ValueType] = double|int|singleString|doubleString|symbol
  def double: Parser[DoubleValue] = floatingPointNumber ^^ {s => DoubleValue(s.toDouble)}
  // TODO: make int actually functional
  def int: Parser[IntegerValue] = wholeNumber ^^ {s => IntegerValue(s.toInt)}
  def innerStringRegex(c: String) = """([^$"""+c+"""\p{Cntrl}\\]|\\[\\'"bfnrt]|\\u[a-fA-F0-9]{4})*+"""
  def parseString(s: String) = StringValue(unescape(s.substring(1, s.length-1)))
  def singleString: Parser[StringValue] = ("\'"+innerStringRegex("\'")+"\'").r ^^ parseString
  def doubleString: Parser[StringValue] = ("\""+innerStringRegex("\"")+"\"").r ^^ parseString
  def symbol: Parser[SymbolValue] = "#" ~> simpleIdent ^^ SymbolValue
  def argument: Parser[ArgumentValue] = "@" ~> simpleIdent ^^ ArgumentValue

  def instruction: Parser[InstructionType] = self|get|bind|set|preBind|preBindObject|obj|infix|infix2|callList|include|ifThenElse|call|closure|openList|closedList|dotOp|manInfix
  def callList: Parser[CallList] = fdent("","\\(") ~ rep(token) <~ ")" ^^ {
    case f ~ c => CallList(f.substring(0,f.length-1), c)
  }

  private def fdent(h: String = "",t: String = "") = (h+"""[:\.]?[_a-zA-Z][\._a-zA-Z0-9]*"""+t).r
//  private def fdent(h: String = "",t: String = "") = (h+"""[:\.]?([!#-&*-\-/:-@\[-`|~]+|[^\s(){}0-9!#-&*-/:-@\[-^`|~]+)(\.([!#-&*-\-/:-@\[-`|~]+|[^\s(){}0-9!#-&*-/:-@\[-^`|~]+))*"""+t).r
  def call: Parser[CallVar] = fdent() ^^ CallVar
  def self: Parser[SelfRef] = opt("$") ~> ("this") ^^ {s => SelfRef(s == "this")}
  def get: Parser[GetVar] = fdent("\\$") ^^ {s => GetVar(s.substring(1))}
  def set: Parser[SetVar] = fdent(">") ^^ {s => SetVar(s.substring(1))}
  def bind: Parser[BindVar] = fdent(">>") ^^ {s => BindVar(s.substring(2))}
  def preBind: Parser[PreBindVar] = fdent("",":") ~ token ^^ {case k~v => PreBindVar(k.substring(0,k.length-1), v)}
  def closure: Parser[PushClosure] = closurePattern() ^^ {case l~c => PushClosure(l.getOrElse(List()),c)}
  def dotOp: Parser[DotOperator] = """\.+""".r ^^ {s => DotOperator(s.length)}
  def ifThenElse: Parser[IfThenElse] = "then" ~ forceWS ~> token ~ opt("else" ~ forceWS ~> token) ^^ {case t~f => IfThenElse(t, f)}
  def infix: Parser[InfixVar] =
    ("+"|"-"|"/"|"*"|"^"|"%"|"to"|
      "="|"!="|"<"|">"|"is"|
      "fold"|"map"|"filter"|"zip"|"join"|
      "until"|"while"|"repeat"|
      "&"|"|"|"dot") ~ forceWS ~ token ^^ {case f~_~x => InfixVar(resolveOp(f), x)}
  def forceWS(): Parser[String] = new Parser[String] {
    def apply(in: Input) = {
      if (in.first == ' ' || in.first == '\n' || in.first == '\t')
        Success("", in)
      else
        Failure("needed whitespace got " + in.first, in)
    }
  }
  // world's hackiest fix contestant number 29:
  def infix2: Parser[InfixVar] = ("<= "|">= "|"<=\n"|">=\n") ~ token ^^ {case f~x => InfixVar(resolveOp(f.substring(0,2)), x)}
  def manInfix: Parser[InfixVar] =
    fdent("@") ~ token ^^ {case f~x => InfixVar(f.substring(1), x)}
  def include: Parser[InfixIncludeVar] = "include " ~> """[_a-zA-Z0-9/\.]+""".r ^^ {t => InfixIncludeVar(t,file)}
  def openList: Parser[OpenList] = "(" ~> rep(token) <~ ")" ^^ OpenList
  def closedList: Parser[ClosedList] = "[" ~> rep(token) <~ "]" ^^ ClosedList
  def obj: Parser[ObjectVar] = closurePattern(":{") ^^ {case l~c => ObjectVar(Closure(l.getOrElse(List()), c, null))}
  def preBindObject: Parser[PreBindVar] = fdent("","\\{") ~ opt(rep(simpleIdent) <~ "=>") ~ rep(token) <~ "}" ^^ {case k~l~c => PreBindVar(k.substring(0, k.length-1),ObjectVar(Closure(l.getOrElse(List()), c, null)))}
}

object InfixTable {
  def reverseLookup(arg: String) = inverseOps.getOrElse(arg, arg)

  val infixOps = Map(
    "+" -> "add", "-" -> "sub", "*" -> "mul", "/" -> "div", "^" -> "pow", "%" -> "mod",
    "=" -> "eq", "!=" -> "neq", "<" -> "lt", ">" -> "mt", "<=" -> "lteq", ">=" -> "mteq",
    "&" -> "and", "|" -> "or"
  )

  val inverseOps = infixOps map (_.swap)
}