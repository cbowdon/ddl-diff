package me.bowdon.ddldiff

sealed trait Sign
case object Plus extends Sign
case object Minus extends Sign

sealed trait Literal
case class NumericLiteral(value: Number) extends Literal
case class SignedNumber(value: NumericLiteral, sign: Sign) extends Literal
case class StringLiteral(value: String) extends Literal
case class BlobLiteral(value: Array[Byte]) extends Literal
case object Null extends Literal
case object CurrentTime extends Literal
case object CurrentDate extends Literal
case object CurrentTimestamp extends Literal
