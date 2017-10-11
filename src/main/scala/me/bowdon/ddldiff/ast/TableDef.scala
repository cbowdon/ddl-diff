package me.bowdon.ddldiff.ast

import me.bowdon.ddldiff._


// https://sqlite.org/syntax/create-table-stmt.html
case class TableDef(
  name: Identifier,
  columns: Map[Identifier, ColumnDef],
  constraints: Set[TableConstraint]) extends SQL {
  override def toSQL() = ???
}

case class ColumnDef(
  name: Identifier,
  sqlType: SQLType,
  constraints: Set[ColumnConstraint]) extends SQL {
  override def toSQL() = ???
}

case class TableConstraint() extends SQL {
  override def toSQL() = ???
}
