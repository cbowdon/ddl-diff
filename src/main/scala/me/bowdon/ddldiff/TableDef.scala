package me.bowdon.ddldiff

// https://sqlite.org/syntax/create-table-stmt.html
case class TableDef(
  name: String,
  columns: Map[String, ColumnDef],
  constraints: Set[TableConstraint])

case class ColumnDef(
  name: String,
  sqlType: SQLType,
  constraints: Set[ColumnConstraint])

case class TableConstraint()
