package me.bowdon.ddldiff

import scala.util.parsing.combinator._
import java.text.NumberFormat

abstract class Sign
case object Plus extends Sign
case object Minus extends Sign

abstract class Literal
case class NumericLiteral(value: Number) extends Literal
case class SignedNumber(value: NumericLiteral, sign: Sign) extends Literal
case class StringLiteral(value: String) extends Literal
case class BlobLiteral(value: Array[Byte]) extends Literal
case object Null extends Literal
case object CurrentTime extends Literal
case object CurrentDate extends Literal
case object CurrentTimestamp extends Literal

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
