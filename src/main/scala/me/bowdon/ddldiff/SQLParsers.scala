package me.bowdon.ddldiff

import scala.util.parsing.combinator._

abstract class Order
case object Asc extends Order
case object Desc extends Order

/**
 * Building blocks for SQL parsing
 */
trait SQLParsers extends RegexParsers {

  /**
   * Case-insensitive parser, i.e. for keywords
   */
  def kw(word: String): Parser[String] = f"(?i)$word".r ^^ { _.toLowerCase }

  // TODO: ANSI quotes
  def identifier: Parser[String] = "[A-Za-z_][0-9A-Za-z_]+".r ^^ { _.toString }

  /**
   * Utility parser for expressions in parentheses
   */
  def parens[T](bodyParser: Parser[T]): Parser[T] = "(" ~> bodyParser <~ ")"

  def order: Parser[Order] = (kw("asc") | kw("desc")) ^^ {
    (_: String) match {
      case "asc" => Asc
      case "desc" => Desc
    }
  }

}
