package me.bowdon.ddldiff

/** MigrationGenerator converts Migrations to DDL statements
 *
 * Main method here is "generate" to actually output the DDL statement Strings.
 * Accompanying methods are "show*" to print bits of AST back to SQL Strings.
 * (Preferring not to override toString in the AST classes because the default
 * implementation is much nicer for debugging.)
 */
object MigrationGenerator {

  def generate(migration: Migration): String = {
    migration match {
      // Tables
      case CreateTable(table) => s"${showTable(table)};"
      case DropTable(table) => s"""drop table "$table";"""
      case RenameTable(oldName, newName) => s"""alter table "$oldName" rename to "$newName";"""

      // Columns
      case AddColumn(table, column) => ???
      case DropColumn(table, column) => ???
      case RenameColumn(table, column) => ???

      // Column constraints
      case AddColumnConstraint(table, column, constraint) => ???
      case DropColumnConstraint(table, column, constraint) => ???

      // Table constraints
      case AddTableConstraint(table, constraint) => ???
      case DropTableConstraint(table, constraint) => ???
    }
  }

  def showTable(table: TableDef): String = {
    val TableDef(name, columns, constraints) = table

    val cols = columns.map(x => showColumn(x._2)).mkString(", ")

    val cons = constraints.map(showTableConstraint).mkString(", ")

    s"create table $name ($cols) $cons".trim
  }

  def showTableConstraint(tableConstraint: TableConstraint): String = ???

  def showColumn(column: ColumnDef): String = {
    val ColumnDef(name, sqlType, constraints) = column

    val cons = constraints
      .map(showColumnConstraint)
      .toSeq
      .sorted // we actually need consistency for the tests
      .mkString(" ")

    s"$name ${sqlType.toString.toLowerCase} $cons".trim
  }

  def showColumnConstraint(constraint: ColumnConstraint): String = {
    constraint match {
      case ColumnConstraint(None, ccDef) => showColumnConstraintDef(ccDef)
      case ColumnConstraint(Some(name), ccDef) => s"constraint $name ${showColumnConstraintDef(ccDef)}"
    }
  }

  def showColumnConstraintDef(constraintDef: ColumnConstraintDef): String = {
    constraintDef match {
      case Unique => "unique"
      case IsNotNull => "not null"
      case Default(literal) => s"default ${showLiteral(literal)}"
      case Collate(collation) => s"collate $collation"
      case Check(expr) => s"check ($expr)"
      case PrimaryKey(orderOpt, autoIncrement) => {
        val order = orderOpt match {
          case None => ""
          case Some(x) => x.toString().toLowerCase()
        }
        val autoInc = if (autoIncrement) "autoincrement" else ""
        Seq("primary key", order, autoInc).filter(x => x != "").mkString(" ")
      }
      case ForeignKey(table, cols) => s"references $table(${cols.mkString(", ")})"
    }
  }

  private def showLiteral(literal: Literal): String = {
    literal match {
      case NumericLiteral(v) => v.toString
      case SignedNumber(v, s) => s"${showSign(s)}v"
      case StringLiteral(v) => s"'$v'"
      case BlobLiteral(v) => v.toString
      case Null => "null"
      case CurrentTime => "current_time"
      case CurrentDate => "current_date"
      case CurrentTimestamp => "current_timestamp"
    }
  }

  private def showSign(sign: Sign): String = sign match {
    case Plus => "+"
    case Minus => "-"
  }
}
