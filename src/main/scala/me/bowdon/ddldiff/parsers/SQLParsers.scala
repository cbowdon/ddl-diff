package me.bowdon.ddldiff.parsers

import scala.util.parsing.combinator._
import me.bowdon.ddldiff.ast._

/**
 * Building blocks for SQL parsing
 */
trait SQLParsers extends RegexParsers {

  /** 
   * Custom string interpolation to make specifying keywords easier
   *
   * This builds a regex parser from the given string that is case-insensitive and ignores whitespace.
   */
  implicit class SQLParserHelper(val sc: StringContext) {
    def p(args: Any*): Parser[String] = {
      val words = sc.s(args:_*)
        .split(" ")
        .filter(x => x.trim != "")
        .mkString("\\s+")

      raw"(?i)\s*$words\s*".r ^^ { _.toLowerCase().trim }
    }
  }

  // TODO: ANSI quotes
  def identifier: Parser[Identifier] = "[A-Za-z_][0-9A-Za-z_]+".r ^^ { x => Identifier(x.toString) }

  /**
   * Utility parser for expressions in parentheses
   */
  def parens[T](bodyParser: Parser[T]): Parser[T] = "(" ~> bodyParser <~ ")"

  def order: Parser[Order] = (p"asc" | p"desc") ^^ {
    (_: String) match {
      case "asc" => Asc
      case "desc" => Desc
    }
  }

}
