package me.bowdon

import scala.util.parsing.combinator._

// Start with SQLite's grammar
abstract class SQLType
case object Number extends SQLType
case object Text extends SQLType

abstract class DDLExpression
case object Create extends DDLExpression
case class Table(name: String) extends DDLExpression
// TODO: constraints
case class Column(name: String, sqlType: SQLType) extends DDLExpression
case class Columns(defs: Column*) extends DDLExpression
case object Terminator extends DDLExpression

class ParseError(reason: String) {
  override def toString = reason
}

class DDLParser extends RegexParsers {

  def create: Parser[DDLExpression] = "(?i)create".r ^^ { _ => Create }

  // TODO: ANSI quotes
  def identifier: Parser[String] = "[A-Za-z_][0-9A-Za-z_]+".r ^^ { _.toString }

  def table: Parser[DDLExpression] = {
    "(?i)table".r ~> identifier ^^ { Table(_) }
  }

  def sqlType: Parser[SQLType] = {
    "(?i)number".r ^^ { _ => Number } |
    "(?i)text".r ^^ { _ => Text }
  }

  def column: Parser[Column] = {
    identifier ~ sqlType ^^ { case name ~ sqlType => Column(name, sqlType) }
  }

  def columns: Parser[DDLExpression] = {
    val colParsers = "(" ~> (column <~ ",".? ).* <~ ")" ^^ identity
    colParsers.map(cols => Columns(cols: _*))
  }

  def terminator: Parser[DDLExpression] = ";" ^^ { _ => Terminator }

  def expr: Parser[Seq[DDLExpression]] = {
    create ~ table ~ columns ~ terminator.? ^^ {
      case create ~ table ~ columns ~ _ => Seq(create, table, columns)
    }
  }

  def apply(input: String): Either[ParseError, Seq[DDLExpression]] = {
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
