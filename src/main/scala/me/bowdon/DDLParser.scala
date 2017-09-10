package me.bowdon

import scala.util.parsing.combinator._
import java.text.NumberFormat

// Start with SQLite's grammar.
// To support multiple vendor grammars, will need to define a superset?

abstract class SQLLiteral
case class NumericLiteral(value: Number) extends SQLLiteral
case class SignedNumber(value: NumericLiteral, sign: Sign) extends SQLLiteral
case class StringLiteral(value: String) extends SQLLiteral
case class BlobLiteral(value: Array[Byte]) extends SQLLiteral
case object Null extends SQLLiteral
case object CurrentTime extends SQLLiteral
case object CurrentDate extends SQLLiteral
case object CurrentTimestamp extends SQLLiteral

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
case class Default(value: SQLLiteral) extends ColumnConstraint
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

class DDLParser extends RegexParsers {

  def create: Parser[String] = "(?i)create( (temp|temporary))?".r

  // TODO: ANSI quotes
  def identifier: Parser[String] = "[A-Za-z_][0-9A-Za-z_]+".r ^^ { _.toString }

  def sqlType: Parser[SQLType] = {
    "(?i)number".r ^^ { _ => Number } |
    "(?i)text".r ^^ { _ => Text }
  }

  def order: Parser[Order] = ("(?i)asc".r | "(?i)desc".r) ^^ {
    (_: String).toLowerCase() match {
      case "asc" => Asc
      case "desc" => Desc
    }
  }

  def primaryKey: Parser[PrimaryKey] = {
    "(?i)primary key".r ~> order.? ~ "(?i)autoincrement".r.? ^^ {
      case order ~ autoincrement => PrimaryKey(order, autoincrement match {
        case Some(_) => true
        case None => false
      })
    }
  }

  def notNull: Parser[ColumnConstraint] = {
    "(?i)not null".r ^^ { _ => IsNotNull }
  }

  def unique: Parser[ColumnConstraint] = {
    "(?i)unique".r ^^ { _ => Unique }
  }

  def collate: Parser[Collate] = {
    "(?i)collate".r ~> identifier ^^ { Collate(_) }
  }

  def numericLiteral: Parser[NumericLiteral] = {
    "[0-9]+".r ^^ { num => NumericLiteral(NumberFormat.getInstance().parse(num)) }
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

  def stringLiteral: Parser[StringLiteral] = "'" ~> "[^']*".r <~ "'" ^^ { StringLiteral(_) }

  def nullLiteral: Parser[SQLLiteral] = "(?i)null".r ^^ { _ => Null }

  def currentTime: Parser[SQLLiteral] = "(?i)current_time".r ^^ { _ => CurrentTime }

  def currentDate: Parser[SQLLiteral] = "(?i)current_date".r ^^ { _ => CurrentDate }

  def currentTimestamp: Parser[SQLLiteral] = "(?i)current_timestamp".r ^^ { _ => CurrentTimestamp }

  def literalValue: Parser[SQLLiteral] = {
    stringLiteral |
    numericLiteral |
    nullLiteral |
    currentTime |
    currentDate |
    currentTimestamp
  }

  def default: Parser[Default] = {
    "(?i)default".r ~> (signedNumber | literalValue) ^^ { Default(_) }
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
    "(?i)table( (if not exists))?".r ~> identifier ~ columns ^^ {
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

// Just to make it do something
object Main extends App {
  val parser = new DDLParser
  val result = parser.apply("create table foo")
  println(result)
}
