package me.bowdon.ddldiff.ast

import me.bowdon.ddldiff.ShowSQL
import ShowSQL._


sealed trait Literal extends SQL {
  override def toSQL() = {
      this match {
        case NumericLiteral(v) => v.toString
        case SignedNumber(v, s) => sql"$s $v"
        case StringLiteral(v) => s"'$v'"
        case BlobLiteral(v) => v.toString
        case Null => "null"
        case CurrentTime => "current_time"
        case CurrentDate => "current_date"
        case CurrentTimestamp => "current_timestamp"
      }
    }
}
case class NumericLiteral(value: Number) extends Literal
case class SignedNumber(value: NumericLiteral, sign: Sign) extends Literal
case class StringLiteral(value: String) extends Literal
case class BlobLiteral(value: Array[Byte]) extends Literal
case object Null extends Literal
case object CurrentTime extends Literal
case object CurrentDate extends Literal
case object CurrentTimestamp extends Literal


sealed trait Sign extends SQL {
  override def toSQL() = {
    this match {
      case Plus => "+"
      case Minus => "-"
    }
  }
}
case object Plus extends Sign
case object Minus extends Sign
