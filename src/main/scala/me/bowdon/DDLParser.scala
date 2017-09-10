package me.bowdon

import scala.util.parsing.combinator._

// Start with SQLite's grammar.
// Need plan to support multiple vendor grammars

// https://sqlite.org/datatype3.html
abstract class SQLType
case object Text extends SQLType
case object Numeric extends SQLType
case object Integer extends SQLType
case object Real extends SQLType
case object Blob extends SQLType

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

  // TODO affinities
  // https://sqlite.org/datatype3.html#determination_of_column_affinity
  def sqlType: Parser[SQLType] = {
    kw("text") ^^ { _ => Text } |
    kw("numeric") ^^ { _ => Numeric } |
    kw("integer") ^^ { _ => Integer } |
    kw("real") ^^ { _ => Real } |
    kw("blob") ^^ { _ => Blob }
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
    // TODO the type is actually optional (defaults to blob with SQLite)
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
