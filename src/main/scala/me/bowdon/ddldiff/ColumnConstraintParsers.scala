package me.bowdon.ddldiff

import scala.util.parsing.combinator._

// https://sqlite.org/syntax/column-constraint.html
sealed trait ColumnConstraintDef
case class PrimaryKey(order: Option[Order], autoIncrement: Boolean) extends ColumnConstraintDef
case object IsNotNull extends ColumnConstraintDef
case object Unique extends ColumnConstraintDef
case class Default(value: Literal) extends ColumnConstraintDef
case class Collate(collationName: String) extends ColumnConstraintDef
case class Check(expr: String) extends ColumnConstraintDef
// TODO "on delete cascade" and friends
// https://sqlite.org/syntax/foreign-key-clause.html
case class ForeignKey(table: String, columns: Seq[String]) extends ColumnConstraintDef

case class ColumnConstraint(name: Option[String], definition: ColumnConstraintDef)

trait ColumnConstraintParsers extends LiteralParsers {

  def primaryKey: Parser[ColumnConstraintDef] = {
    p"primary key" ~> order.? ~ p"autoincrement".? ^^ {
      case order ~ autoincrement => PrimaryKey(order, autoincrement match {
        case Some(_) => true
        case None => false
      })
    }
  }

  def notNull: Parser[ColumnConstraintDef] = {
    p"not null" ^^ { _ => IsNotNull }
  }

  def unique: Parser[ColumnConstraintDef] = {
    p"unique" ^^ { _ => Unique }
  }

  def collate: Parser[ColumnConstraintDef] = {
    p"collate" ~> identifier ^^ { Collate(_) }
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

  def default: Parser[ColumnConstraintDef] = {
    p"default" ~> (signedNumber | literalValue) ^^ { Default(_) }
  }

  def check: Parser[ColumnConstraintDef] = {
    // TODO this is a buggy cheat: needs to be a complete expr parser here
    p"check" ~> parens("[^)]+".r) ^^ { Check(_) }
  }

  def foreignKey: Parser[ColumnConstraintDef] = {
    p"references" ~> identifier ~ parens(repsep(identifier, ",")).? ^^ {
      case table ~ columns => ForeignKey(table, columns match {
        case Some(cols) => cols
        case None => Seq.empty
      })
    }
  }

  def constraintDef: Parser[ColumnConstraintDef] = {
    primaryKey | notNull | unique | collate | default | check | foreignKey
  }

  def constraintName: Parser[String] = p"constraint" ~> identifier

  def columnConstraint: Parser[ColumnConstraint] = {
    constraintName.? ~ constraintDef ^^ {
      case name ~ cdef => ColumnConstraint(name, cdef)
    }
  }
}
