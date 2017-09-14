package me.bowdon

abstract class Migration

// Table migrations
case class CreateTable(table: TableDef) extends Migration
case class DropTable(tableName: String) extends Migration
case class RenameTable(oldTableName: String, newTableName: String) extends Migration

// Column migrations
case class AddColumn(tableName: String, column: ColumnDef) extends Migration
case class DropColumn(tableName: String, column: ColumnDef) extends Migration
case class RenameColumn(tableName: String, column: ColumnDef) extends Migration

// Column constraint migrations
case class AddColumnConstraint(tableName: String, columnName: String, constraint: ColumnConstraint) extends Migration
case class DropColumnConstraint(tableName: String, columnName: String, constraint: ColumnConstraint) extends Migration

// Table constraint migrations
case class AddTableConstraint(tableName: String, constraint: TableConstraint) extends Migration
case class DropTableConstraint(tableName: String, constraint: TableConstraint) extends Migration

object DiffCalculator {

  def diff(oldTable: Option[TableDef], newTable: Option[TableDef]): Seq[Migration] = {
    (oldTable, newTable) match {
      case (None, None) => Seq()
      case (None, Some(table)) => Seq(CreateTable(table))
      case (Some(table), None) => Seq(DropTable(table.name))
      case (Some(oldTab), Some(newTab)) => {

        val added = newTab.columns.keySet -- oldTab.columns.keySet
        val dropped = oldTab.columns.keySet -- newTab.columns.keySet
        val remaining = oldTab.columns.keySet & newTab.columns.keySet

        val addColumns = added.map(col => AddColumn(newTab.name, newTab.columns(col)))
        val dropColumns = dropped.map(col => DropColumn(oldTab.name, oldTab.columns(col)))

        addColumns.toSeq ++ dropColumns.toSeq
      }
    }
  }
}
