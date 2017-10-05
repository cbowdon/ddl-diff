package me.bowdon.ddldiff

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

        val columnConstraints = remaining.flatMap(col => {
          diffColumnConstraints(newTab.name, oldTab.columns(col), newTab.columns(col))
        })

        dropColumns.toSeq ++ addColumns.toSeq ++ columnConstraints
      }
    }
  }

  def diffColumnConstraints(tableName: String, oldCol: ColumnDef, newCol: ColumnDef): Seq[Migration] = {
    val added = newCol.constraints -- oldCol.constraints
    val dropped = oldCol.constraints -- newCol.constraints

    val addConstraints = added.map(constraint => AddColumnConstraint(tableName, newCol.name, constraint))
    val dropConstraints = dropped.map(constraint => DropColumnConstraint(tableName, oldCol.name, constraint))

    dropConstraints.toSeq ++ addConstraints.toSeq
  }
}
