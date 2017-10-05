package me.bowdon.ddldiff

import scala.util.parsing.combinator._
import java.text.NumberFormat

trait LiteralParsers extends SQLParsers {

  def numericLiteral: Parser[NumericLiteral] = {
    "[0-9]+\\.?[0-9]*".r ^^ { num => NumericLiteral(NumberFormat.getInstance().parse(num)) }
  }

  // TODO escaping quotes
  def stringLiteral: Parser[StringLiteral] = "'" ~> "[^']*".r <~ "'" ^^ { StringLiteral(_) }

  def nullLiteral: Parser[Literal] = p"null" ^^ { _ => Null }

  def currentTime: Parser[Literal] = p"current_time" ^^ { _ => CurrentTime }

  def currentDate: Parser[Literal] = p"current_date" ^^ { _ => CurrentDate }

  def currentTimestamp: Parser[Literal] = p"current_timestamp" ^^ { _ => CurrentTimestamp }

  def literalValue: Parser[Literal] = {
    stringLiteral |
      numericLiteral |
      nullLiteral |
      currentDate |
      currentTimestamp |
      currentTime // Timestamp must go before time or input not consumed!
  }
}
