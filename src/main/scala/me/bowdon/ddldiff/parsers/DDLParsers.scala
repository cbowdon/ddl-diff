package me.bowdon.ddldiff.parsers

import me.bowdon.ddldiff.ast._

class ParseError(reason: String) {
  override def toString = reason
}

object DDLParsers extends ColumnConstraintParsers {

  def create: Parser[String] = p"create" <~ (p"temp" | p"temporary").?

  // TODO affinities
  // https://sqlite.org/datatype3.html#determination_of_column_affinity
  def sqlType: Parser[SQLType] = {
    p"text" ^^ { _ => Text } |
      p"numeric" ^^ { _ => Numeric } |
      p"integer" ^^ { _ => Integer } |
      p"real" ^^ { _ => Real } |
      p"blob" ^^ { _ => Blob }
  }

  def column: Parser[ColumnDef] = {
    // TODO the type is actually optional (defaults to blob with SQLite)
    identifier ~ sqlType ~ columnConstraint.* ^^ {
      case name ~ sqlType ~ colConstraints => ColumnDef(name, sqlType, colConstraints.toSet)
    }
  }

  def columns: Parser[Map[Identifier, ColumnDef]] = {

    val parser = parens(repsep(column, ","))

    parser.map(colDefs =>
      colDefs.map(col => (col.name, col)).toMap)
  }

  def table: Parser[TableDef] = {
    p"table" ~ p"if not exists".? ~> identifier ~ columns ^^ {
      case name ~ cols => TableDef(name, cols, Set.empty)
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
