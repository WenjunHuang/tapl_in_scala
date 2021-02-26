package tapl.parser

import scala.util.parsing.combinator.RegexParsers

sealed trait Token
case object LAMBDA extends Token
case object LPAREN extends Token
case object RPAREN extends Token
case object PLUS extends Token
object lexer extends RegexParsers {
  def lambda: Parser[LAMBDA.type] = "lambda" ^^ { _ => LAMBDA }
  def lparen: Parser[LPAREN.type] = "(" ^^ { _ => LPAREN }

}
