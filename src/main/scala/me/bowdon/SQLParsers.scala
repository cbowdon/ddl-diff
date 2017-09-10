package me.bowdon

import scala.util.parsing.combinator._

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

}
