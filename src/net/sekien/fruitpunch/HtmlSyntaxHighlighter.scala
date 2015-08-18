package net.sekien.fruitpunch

class HtmlSyntaxHighlighter extends SyntaxHighlighter {
  def wrap(c: String, s: String): String = s"""<span class="$c">$s</span>"""
  override def call(s: String): String = wrap("fn",s)
  override def sym(s: String): String = wrap("sym",s)
  override def str(s: String): String = wrap("string",s)
  override def num(s: String): String = wrap("num",s)
  override def infix(s: String): String = wrap("keyword",s)
  override def arg(s: String): String = wrap("arg",s)
  override def variable(s: String): String = wrap("var",s)
}
