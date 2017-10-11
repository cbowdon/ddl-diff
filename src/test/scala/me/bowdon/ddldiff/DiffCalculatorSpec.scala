package me.bowdon.ddldiff

import org.scalatest._
import org.scalatest.Matchers._
import me.bowdon.ddldiff.ast._

class DiffCalculatorSpec extends FlatSpec with Matchers {

  "DiffCalculator" should "create missing tables" in {

    val idCol = ColumnDef(Identifier("id"), Numeric, Set.empty)
    val newTable = TableDef(Identifier("foo"), Map(Identifier("id") -> idCol), Set())

    DiffCalculator.diff(None, Some(newTable)) shouldEqual
      Seq(
        CreateTable(newTable))
  }

  it should "drop removed tables" in {

    val idCol = ColumnDef(Identifier("id"), Numeric, Set.empty)
    val oldTable = TableDef(Identifier("foo"), Map(Identifier("id") -> idCol), Set())

    DiffCalculator.diff(Some(oldTable), None) shouldEqual
      Seq(
        DropTable(Identifier("foo")))
  }

  it should "do nothing for nothing" in {

    DiffCalculator.diff(None, None) shouldEqual Seq()
  }

  it should "add missing columns" in {

    val oldTable = TableDef(Identifier("foo"), Map(), Set())

    val idCol = ColumnDef(Identifier("id"), Numeric, Set.empty)
    val newTable = oldTable.copy(columns = Map(Identifier("id") -> idCol))

    DiffCalculator.diff(Some(oldTable), Some(newTable)) shouldEqual
      Seq(
        AddColumn(Identifier("foo"), idCol))
  }

  it should "drop removed columns" in {

    val idCol = ColumnDef(Identifier("id"), Numeric, Set.empty)
    val oldTable = TableDef(Identifier("foo"), Map(Identifier("id") -> idCol), Set())

    val newTable = oldTable.copy(columns = Map())

    DiffCalculator.diff(Some(oldTable), Some(newTable)) shouldEqual
      Seq(
        DropColumn(Identifier("foo"), idCol))
  }

  it should "add new column constraints" in {

    val oldIdCol = ColumnDef(Identifier("id"), Numeric, Set.empty)
    val constraint = ColumnConstraint(None, PrimaryKey(Some(Asc), true))
    val newIdCol = oldIdCol.copy(constraints = Set(constraint))

    val oldTable = TableDef(Identifier("foo"), Map(Identifier("id") -> oldIdCol), Set())

    val newTable = oldTable.copy(columns = Map(Identifier("id") -> newIdCol))

    DiffCalculator.diff(Some(oldTable), Some(newTable)) shouldEqual
      Seq(
        AddColumnConstraint(Identifier("foo"), Identifier("id"), constraint))
  }

  it should "drop removed column constraints" in {

    val constraint = ColumnConstraint(None, PrimaryKey(Some(Asc), true))
    val oldIdCol = ColumnDef(Identifier("id"), Numeric, Set(constraint))
    val newIdCol = oldIdCol.copy(constraints = Set())

    val oldTable = TableDef(Identifier("foo"), Map(Identifier("id") -> oldIdCol), Set())

    val newTable = oldTable.copy(columns = Map(Identifier("id") -> newIdCol))

    DiffCalculator.diff(Some(oldTable), Some(newTable)) shouldEqual
      Seq(
        DropColumnConstraint(Identifier("foo"), Identifier("id"), constraint))
  }

  it should "assume a constraint rename is a drop and create" in {

    val oldConstraint = ColumnConstraint(Some(Identifier("foo_pk")), PrimaryKey(Some(Asc), true))
    val newConstraint = ColumnConstraint(Some(Identifier("foo_pkx")), PrimaryKey(Some(Asc), true))
    val idCol = ColumnDef(Identifier("id"), Numeric, Set(oldConstraint))
    val oldTable = TableDef(Identifier("foo"), Map(Identifier("id") -> idCol), Set())
    val newTable = oldTable.copy(
      columns = Map(Identifier("id") -> idCol.copy(
        constraints = Set(newConstraint))))

    DiffCalculator.diff(Some(oldTable), Some(newTable)) shouldEqual
      Seq(
        DropColumnConstraint(Identifier("foo"), Identifier("id"), oldConstraint),
        AddColumnConstraint(Identifier("foo"), Identifier("id"), newConstraint))
  }
}
