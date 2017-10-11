package me.bowdon.ddldiff

import org.scalatest._
import org.scalatest.Matchers._
import org.scalatest.prop.TableDrivenPropertyChecks._
import me.bowdon.ddldiff.ast._

class MigrationGeneratorSpec extends FlatSpec with Matchers {

  "MigrationGenerator" should "be able to generate a complete table definition" in {

    val createTableMigration =
      CreateTable(
        TableDef(
          Identifier("foo"),
          Map(
            Identifier("id") -> ColumnDef(
              Identifier("id"),
              Integer,
              Set(
                // Named constraint
                ColumnConstraint(
                  Some(Identifier("PK_Foo_id")),
                  PrimaryKey(Some(Asc), true)))),
            // Multiple columns
            Identifier("name") -> ColumnDef(
              Identifier("name"),
              Text,
              Set(
                // Unnamed constraint
                ColumnConstraint(None, IsNotNull),
                // Multiple constraints on single column
                ColumnConstraint(Some(Identifier("Uniq_Foo_name")), Unique),
                // Foreign key constraint with single other table column
                ColumnConstraint(Some(Identifier("FK_Foo_name")), ForeignKey(Identifier("other"), Seq(Identifier("name")))))),
            Identifier("description") -> ColumnDef(
              Identifier("description"),
              Text,
              Set(
                // All the other constraints
                ColumnConstraint(None, Default(StringLiteral("A thing!"))),
                ColumnConstraint(None, Check("1 > 0")),
                ColumnConstraint(None, Collate(Identifier("binary")))))),
          // TODO table constraints
          Set.empty))

    MigrationGenerator.generate(createTableMigration) shouldEqual
    "create table Foo (id integer constraint PK_Foo_id primary key asc autoincrement, name text constraint FK_Foo_name references other(name) constraint Uniq_Foo_name unique not null, description text check (1 > 0) collate binary default 'A thing!');"
  }

  it should "show column defs" in {
    val columnDefs =
      Table(
        ("columnDef", "sqlOutput"),
        (ColumnDef(Identifier("foo"), Text, Set()), "foo text"),
        (ColumnDef(Identifier("foo"), Blob, Set(ColumnConstraint(None, Unique), ColumnConstraint(None, IsNotNull))), "foo blob not null unique"))

    forAll(columnDefs) {
      (columnDef: ColumnDef, sqlOutput: String) => {
        MigrationGenerator.showColumn(columnDef) shouldEqual sqlOutput
      }
    }
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

        (ForeignKey(Identifier("other"), Seq(Identifier("name"))), "references other(name)"),
        (IsNotNull, "not null"),
        (Unique, "unique"),

        // Default variations
        (Default(StringLiteral("A thing!")), "default 'A thing!'"),
        (Default(NumericLiteral(42)), "default 42"),

        (Check("1 > 0"), "check (1 > 0)"),
        (Collate(Identifier("binary")), "collate binary")
      )

    forAll(constraintDefs) {
      (constraintDef: ColumnConstraintDef, sqlOutput: String) => {
        constraintDef.toSQL shouldEqual sqlOutput
      }
    }
  }

  // TODO toSQL tests should move to own spec
  it should "show column constraints" in {
    val constraints =
      Table(
        ("constraint", "sqlOutput"),
        (ColumnConstraint(None, PrimaryKey(None, false)), "primary key"),
        (ColumnConstraint(Some(Identifier("PK_foo_id")), PrimaryKey(None, false)), "constraint PK_foo_id primary key"))

    forAll(constraints) {
      (constraint: ColumnConstraint, sqlOutput: String) => {
        constraint.toSQL shouldEqual sqlOutput
      }
    }
  }
}
