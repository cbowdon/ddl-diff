package me.bowdon

import scala.util.parsing.combinator._

// https://sqlite.org/syntax/column-constraint.html
abstract class ColumnConstraint // TODO name: Option[String]
case class PrimaryKey(order: Option[Order], autoIncrement: Boolean) extends ColumnConstraint
case object IsNotNull extends ColumnConstraint
case object Unique extends ColumnConstraint
case class Default(value: Literal) extends ColumnConstraint
case class Collate(collationName: String) extends ColumnConstraint
case class Check(/* TODO */) extends ColumnConstraint
case class ForeignKey(/* TODO */) extends ColumnConstraint

trait ColumnConstraintParsers extends LiteralParsers {

  def primaryKey: Parser[PrimaryKey] = {
    kw("primary") ~ kw("key") ~> order.? ~ kw("autoincrement").? ^^ {
      case order ~ autoincrement => PrimaryKey(order, autoincrement match {
        case Some(_) => true
        case None => false
      })
    }
  }

  def notNull: Parser[ColumnConstraint] = {
    kw("not") ~ kw("null") ^^ { _ => IsNotNull }
  }

  def unique: Parser[ColumnConstraint] = {
    kw("unique") ^^ { _ => Unique }
  }

  def collate: Parser[Collate] = {
    kw("collate") ~> identifier ^^ { Collate(_) }
  }

  def signedNumber: Parser[SignedNumber] = {
    ("-" | "+") ~ numericLiteral ^^ {
      case sign ~ num => {
        SignedNumber(num, sign match {
          case "+" => Plus
          case "-" => Minus
        })
      }
    }
  }

  def default: Parser[Default] = {
    kw("default") ~> (signedNumber | literalValue) ^^ { Default(_) }
  }

  def columnConstraint: Parser[ColumnConstraint] = {
    primaryKey | notNull | unique | collate | default
  }
}
