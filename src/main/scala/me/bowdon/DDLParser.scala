package me.bowdon

// Start with SQLite's grammar.
// Need plan to support multiple vendor grammars

// https://sqlite.org/datatype3.html
abstract class SQLType
case object Text extends SQLType
case object Numeric extends SQLType
case object Integer extends SQLType
case object Real extends SQLType
case object Blob extends SQLType

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

object DDLParser extends ColumnConstraintParsers {

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
