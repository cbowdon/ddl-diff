package me.bowdon.ddldiff

import org.scalatest._
import org.scalatest.Matchers._

class DiffCalculatorSpec extends FlatSpec with Matchers {

  "DiffCalculator" should "create missing tables" in {

    val idCol = ColumnDef("id", Numeric, Set.empty)
    val newTable = TableDef("foo", Map("id" -> idCol), Set())

    DiffCalculator.diff(None, Some(newTable)) shouldEqual
    Seq(
      CreateTable(newTable))
  }

  it should "drop removed tables" in {

    val idCol = ColumnDef("id", Numeric, Set.empty)
    val oldTable = TableDef("foo", Map("id" -> idCol), Set())

    DiffCalculator.diff(Some(oldTable), None) shouldEqual
    Seq(
      DropTable("foo")
    )
  }

  it should "do nothing for nothing" in {

    DiffCalculator.diff(None, None) shouldEqual Seq()
  }

  it should "add missing columns" in {

    val oldTable = TableDef("foo", Map(), Set())

    val idCol = ColumnDef("id", Numeric, Set.empty)
    val newTable = oldTable.copy(columns = Map("id" -> idCol))

    DiffCalculator.diff(Some(oldTable), Some(newTable)) shouldEqual
    Seq(
      AddColumn("foo", idCol)
    )
  }

  it should "drop removed columns" in {

    val idCol = ColumnDef("id", Numeric, Set.empty)
    val oldTable = TableDef("foo", Map("id" -> idCol), Set())

    val newTable = oldTable.copy(columns = Map())

    DiffCalculator.diff(Some(oldTable), Some(newTable)) shouldEqual
    Seq(
      DropColumn("foo", idCol)
    )
  }

  it should "add new column constraints" in {

    val oldIdCol = ColumnDef("id", Numeric, Set.empty)
    val constraint = ColumnConstraint(None, PrimaryKey(Some(Asc), true))
    val newIdCol = oldIdCol.copy(constraints = Set(constraint))

    val oldTable = TableDef("foo", Map("id" -> oldIdCol), Set())

    val newTable = oldTable.copy(columns = Map("id" -> newIdCol))

    DiffCalculator.diff(Some(oldTable), Some(newTable)) shouldEqual
    Seq(
      AddColumnConstraint("foo", "id", constraint)
    )
  }

  it should "drop removed column constraints" in {

    val constraint = ColumnConstraint(None, PrimaryKey(Some(Asc), true))
    val oldIdCol = ColumnDef("id", Numeric, Set(constraint))
    val newIdCol = oldIdCol.copy(constraints = Set())

    val oldTable = TableDef("foo", Map("id" -> oldIdCol), Set())

    val newTable = oldTable.copy(columns = Map("id" -> newIdCol))

    DiffCalculator.diff(Some(oldTable), Some(newTable)) shouldEqual
    Seq(
      DropColumnConstraint("foo", "id", constraint)
    )
  }

  it should "assume a constraint rename is a drop and create" in {

    val oldConstraint = ColumnConstraint(Some("foo_pk"), PrimaryKey(Some(Asc), true))
    val newConstraint = ColumnConstraint(Some("foo_pkx"), PrimaryKey(Some(Asc), true))
    val idCol = ColumnDef("id", Numeric, Set(oldConstraint))
    val oldTable = TableDef("foo", Map("id" -> idCol), Set())
    val newTable = oldTable.copy(
      columns = Map("id" -> idCol.copy(
        constraints = Set(newConstraint))
      )
    )

    DiffCalculator.diff(Some(oldTable), Some(newTable)) shouldEqual
    Seq(
      DropColumnConstraint("foo", "id", oldConstraint),
      AddColumnConstraint("foo", "id", newConstraint)
    )
  }
}
