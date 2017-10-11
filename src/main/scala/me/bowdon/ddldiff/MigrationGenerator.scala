package me.bowdon.ddldiff

import me.bowdon.ddldiff.ast._

/** MigrationGenerator converts Migrations to DDL statements
 *
 * Main method here is "generate" to actually output the DDL statement Strings.
 * Accompanying methods are "show*" to print bits of AST back to SQL Strings.
 * (Preferring not to override toString in the AST classes because the default
 * implementation is much nicer for debugging.)
 */
object MigrationGenerator {

  import ShowSQL._

  def generate(migration: Migration): String = {
    migration match {
      // Tables
      case CreateTable(table) => s"${showTable(table)};"
      case DropTable(table) => s"""drop table "$table";"""
      case RenameTable(oldName, newName) => sql"""alter table "$oldName" rename to "$newName";"""

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
      .map(cc => sql"$cc")
      .toSeq
      .sorted // we actually need consistency for the tests
      .mkString(" ")

    sql"$name $sqlType $cons".trim
  }
}
