package net.sekien.fruitpunch

trait SyntaxHighlighter {
  def call(s: String):String
  def num(s: String):String
  def sym(s: String):String
  def variable(s: String):String
  def str(s: String):String
  def infix(s: String):String
  def arg(s: String):String
}
