package me.bowdon

import scala.util.parsing.combinator._

// Start with SQLite's grammar.
// To support multiple vendor grammars, will need to define a superset?

abstract class Sign
case object Plus extends Sign
case object Minus extends Sign

abstract class SQLType
case object Number extends SQLType
case object Text extends SQLType

abstract class Order
case object Asc extends Order
case object Desc extends Order

// https://sqlite.org/syntax/column-constraint.html
abstract class ColumnConstraint // TODO name: Option[String]
case class PrimaryKey(order: Option[Order], autoIncrement: Boolean) extends ColumnConstraint
case object IsNotNull extends ColumnConstraint
case object Unique extends ColumnConstraint
case class Default(value: Literal) extends ColumnConstraint
case class Collate(collationName: String) extends ColumnConstraint
case class Check(/* TODO */) extends ColumnConstraint
case class ForeignKey(/* TODO */) extends ColumnConstraint

case class TableConstraint()

case class ColumnDef(
  name: String,
  sqlType: SQLType,
  constraints: Seq[ColumnConstraint])

// https://sqlite.org/syntax/create-table-stmt.html
case class TableDef(
  name: String,
  columns: Seq[ColumnDef],
  constraints: Seq[TableConstraint])

class ParseError(reason: String) {
  override def toString = reason
}

object DDLParser extends LiteralParsers {

  def create: Parser[String] = kw("create") <~ (kw("temp") | kw("temporary")).?

  def sqlType: Parser[SQLType] = {
    kw("number") ^^ { _ => Number } |
    kw("text") ^^ { _ => Text }
  }

  def order: Parser[Order] = (kw("asc") | kw("desc")) ^^ {
    (_: String) match {
      case "asc" => Asc
      case "desc" => Desc
    }
  }

  def primaryKey: Parser[PrimaryKey] = {
    kw("primary key") ~> order.? ~ kw("autoincrement").? ^^ {
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

  def column: Parser[ColumnDef] = {
    identifier ~ sqlType ~ columnConstraint.* ^^ {
      case name ~ sqlType ~ colConstraints => ColumnDef(name, sqlType, colConstraints)
    }
  }

  def columns: Parser[Seq[ColumnDef]] = "(" ~> (column <~ ",".? ).* <~ ")"

  def table: Parser[TableDef] = {
    kw("table") ~ (kw("if") ~ kw("not") ~ kw("exists")).? ~> identifier ~ columns ^^ {
      case name ~ cols => TableDef(name, cols, Seq.empty)
    }
  }

  def terminator: Parser[String] = ";"

  def expr: Parser[TableDef] = {
    create ~> table <~ terminator.?
  }

  def apply(input: String): Either[ParseError, TableDef] = {
    // Either is standing in for a type I have yet to design
    parseAll(expr, input) match {
      case Success(result, _) => Right(result)
      case failure: NoSuccess => Left(new ParseError(failure.msg))
    }
  }
}
