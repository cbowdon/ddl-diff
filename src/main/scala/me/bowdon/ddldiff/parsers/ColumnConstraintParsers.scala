package me.bowdon.ddldiff.parsers

import me.bowdon.ddldiff.ast._

import scala.util.parsing.combinator._

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

  def constraintName: Parser[Identifier] = p"constraint" ~> identifier

  def columnConstraint: Parser[ColumnConstraint] = {
    constraintName.? ~ constraintDef ^^ {
      case name ~ cdef => ColumnConstraint(name, cdef)
    }
  }
}
