package me.bowdon.ddldiff

import org.scalatest._
import org.scalatest.Matchers._
import org.scalatest.prop.TableDrivenPropertyChecks._

class MigrationGeneratorSpec extends FlatSpec with Matchers {

  "MigrationGenerator" should "be able to generate a complete table definition" in {

    val createTableMigration =
      CreateTable(
        TableDef(
          "Foo",
          Map(
            "id" -> ColumnDef(
              "id",
              Integer,
              Set(
                // Named constraint
                ColumnConstraint(
                  Some("PK_Foo_id"),
                  PrimaryKey(Some(Asc), true)))),
            // Multiple columns
            "name" -> ColumnDef(
              "name",
              Text,
              Set(
                // Unnamed constraint
                ColumnConstraint(None, IsNotNull),
                // Multiple constraints on single column
                ColumnConstraint(Some("Uniq_Foo_name"), Unique),
                // Foreign key constraint with single other table column
                ColumnConstraint(Some("FK_Foo_name"), ForeignKey("other", Seq("name"))))),
            "description" -> ColumnDef(
              "description",
              Text,
              Set(
                // All the other constraints
                ColumnConstraint(None, Default(StringLiteral("A thing!"))),
                ColumnConstraint(None, Check("1 > 0")),
                ColumnConstraint(None, Collate("binary"))))),
          // TODO table constraints
          Set.empty))

    MigrationGenerator.generate(createTableMigration) shouldEqual
    """create table Foo
( id integer constraint PK_Foo_id primary key asc autoincrement
, name text not null constraint Uniq_Foo_name unique constraint FK_Foo_name references other(name)
, description text default 'A thing!' check (1 > 0) collate binary );"""
  }

  it should "show column constraint defs" in {
    val constraintDefs =
      Table(
        ("constraintDef", "sqlOutput"),
        // Primary key variations
        (PrimaryKey(Some(Asc), true), "primary key asc autoincrement"),
        (PrimaryKey(Some(Desc), true), "primary key desc autoincrement"),
        (PrimaryKey(None, true), "primary key autoincrement"),
        (PrimaryKey(Some(Asc), false), "primary key asc"),
        (PrimaryKey(Some(Desc), false), "primary key desc"),
        (PrimaryKey(None, false), "primary key"),

        (ForeignKey("other", Seq("name")), "references other(name)"),
        (IsNotNull, "not null"),
        (Unique, "unique"),

        // Default variations
        (Default(StringLiteral("A thing!")), "default 'A thing!'"),
        (Default(NumericLiteral(42)), "default 42"),

        (Check("1 > 0"), "check (1 > 0)"),
        (Collate("binary"), "collate binary")
      )

    forAll(constraintDefs) {
      (constraintDef: ColumnConstraintDef, sqlOutput: String) => {
        MigrationGenerator.showColumnConstraintDef(constraintDef) shouldEqual sqlOutput
      }
    }
  }

  it should "show column constraints" in {
    val constraints =
      Table(
        ("constraint", "sqlOutput"),
        (ColumnConstraint(None, PrimaryKey(None, false)), "primary key"),
        (ColumnConstraint(Some("PK_foo_id"), PrimaryKey(None, false)), "constraint PK_foo_id primary key"))

    forAll(constraints) {
      (constraint: ColumnConstraint, sqlOutput: String) => {
        MigrationGenerator.showColumnConstraint(constraint) shouldEqual sqlOutput
      }
    }
  }
}
