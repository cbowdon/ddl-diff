package me.bowdon.ddldiff

import me.bowdon.ddldiff.ast._

sealed trait Migration

// Table migrations
case class CreateTable(table: TableDef) extends Migration
case class DropTable(tableName: Identifier) extends Migration
case class RenameTable(oldTableName: Identifier, newTableName: Identifier) extends Migration

// Column migrations
case class AddColumn(tableName: Identifier, column: ColumnDef) extends Migration
case class DropColumn(tableName: Identifier, column: ColumnDef) extends Migration
case class RenameColumn(tableName: Identifier, column: ColumnDef) extends Migration

// Column constraint migrations
case class AddColumnConstraint(tableName: Identifier, columnName: Identifier, constraint: ColumnConstraint) extends Migration
case class DropColumnConstraint(tableName: Identifier, columnName: Identifier, constraint: ColumnConstraint) extends Migration

// Table constraint migrations
case class AddTableConstraint(tableName: Identifier, constraint: TableConstraint) extends Migration
case class DropTableConstraint(tableName: Identifier, constraint: TableConstraint) extends Migration
