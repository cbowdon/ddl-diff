package me.bowdon

import scala.util.parsing.combinator._
import java.text.NumberFormat

abstract class Literal
case class NumericLiteral(value: Number) extends Literal
case class SignedNumber(value: NumericLiteral, sign: Sign) extends Literal
case class StringLiteral(value: String) extends Literal
case class BlobLiteral(value: Array[Byte]) extends Literal
case object Null extends Literal
case object CurrentTime extends Literal
case object CurrentDate extends Literal
case object CurrentTimestamp extends Literal

trait LiteralParsers extends RegexParsers {

  def numericLiteral: Parser[NumericLiteral] = {
    "[0-9]+\\.?[0-9]*".r ^^ { num => NumericLiteral(NumberFormat.getInstance().parse(num)) }
  }

  // TODO escaping quotes
  def stringLiteral: Parser[StringLiteral] = "'" ~> "[^']*".r <~ "'" ^^ { StringLiteral(_) }

  def nullLiteral: Parser[Literal] = "(?i)null".r ^^ { _ => Null }

  def currentTime: Parser[Literal] = "(?i)current_time".r ^^ { _ => CurrentTime }

  def currentDate: Parser[Literal] = "(?i)current_date".r ^^ { _ => CurrentDate }

  def currentTimestamp: Parser[Literal] = "(?i)current_timestamp".r ^^ { _ => CurrentTimestamp }

  def literalValue: Parser[Literal] = {
    stringLiteral |
    numericLiteral |
    nullLiteral |
    currentDate |
    currentTimestamp |
    currentTime // Timestamp must go before time or input not consumed!
  }
}
