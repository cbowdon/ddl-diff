package me.bowdon.ddldiff.parsers

import org.scalatest._
import org.scalatest.Matchers._
import me.bowdon.ddldiff.ast._

class DDLParsersSpec extends FlatSpec with Matchers {

  "The DDLParser class" should "parse a simple create" in {

    DDLParsers.apply("create table foo (id numeric);") shouldEqual
      Right(
        TableDef(
          Identifier("foo"),
          Map(Identifier("id") -> ColumnDef(Identifier("id"), Numeric, Set.empty)),
          Set.empty))
  }

  it should "parse multiple columns" in {

    DDLParsers.apply("create table foo (id numeric, name text);") shouldEqual
      Right(
        TableDef(
          Identifier("foo"),
          Map(
            Identifier("id") -> ColumnDef(Identifier("id"), Numeric, Set.empty),
            Identifier("name") -> ColumnDef(Identifier("name"), Text, Set.empty)),
          Set.empty))
  }

  it should "not parse multiple columns with trailing comma" in {

    DDLParsers.apply("create table foo (id numeric, name text,);") should be('left)
  }

  it should "parse single column constraints" in {

    DDLParsers.apply("create table foo (id numeric primary key asc autoincrement);") shouldEqual
      Right(
        TableDef(
          Identifier("foo"),
          Map(Identifier("id") -> ColumnDef(Identifier("id"), Numeric, Set(ColumnConstraint(None, PrimaryKey(Some(Asc), true))))),
          Set.empty))
  }

  it should "parse multiple column constraints" in {

    DDLParsers.apply("""
create table foo (
  id numeric primary key
, name text constraint foo_unique_name_constraint unique not null collate nocase
, size numeric default 0
);""") shouldEqual
      Right(
        TableDef(
          Identifier("foo"),
          Map(
            Identifier("id") -> ColumnDef(Identifier("id"), Numeric, Set(ColumnConstraint(None, PrimaryKey(None, false)))),
            Identifier("name") -> ColumnDef(Identifier("name"), Text,
              Set(
                ColumnConstraint(Some(Identifier("foo_unique_name_constraint")), Unique),
                ColumnConstraint(None, IsNotNull),
                ColumnConstraint(None, Collate(Identifier("nocase"))))),
            Identifier("size") -> ColumnDef(Identifier("size"), Numeric, Set(ColumnConstraint(None, Default(NumericLiteral(0)))))),
          Set.empty))
  }

  it should "ignore modifiers like 'if not exists'" in {

    DDLParsers.apply("create table if not exists foo (id numeric);") shouldEqual
      Right(
        TableDef(
          Identifier("foo"),
          Map(Identifier("id") -> ColumnDef(Identifier("id"), Numeric, Set.empty)),
          Set.empty))
  }

  it should "ignore modifiers like 'temp'" in {

    DDLParsers.apply("create temp table if not exists foo (id numeric);") shouldEqual
      Right(
        TableDef(
          Identifier("foo"),
          Map(Identifier("id") -> ColumnDef(Identifier("id"), Numeric, Set.empty)),
          Set.empty))
  }

  it should "not care if people are shouting" in {

    DDLParsers.apply("CREATE TABLE foo (id NUMERIC);") shouldEqual
      Right(
        TableDef(
          Identifier("foo"),
          Map(Identifier("id") -> ColumnDef(Identifier("id"), Numeric, Set.empty)),
          Set.empty))
  }

  it should "not care about messy whitespace" in {

    DDLParsers.apply("CREATE     TABLE  foo    ( id\t\tNUMERIC );") shouldEqual
      Right(
        TableDef(
          Identifier("foo"),
          Map(Identifier("id") -> ColumnDef(Identifier("id"), Numeric, Set.empty)),
          Set.empty))
  }

  it should "not mind a missing terminator" in {

    DDLParsers.apply("CREATE TABLE foo (id NUMERIC)") shouldEqual
      Right(
        TableDef(
          Identifier("foo"),
          Map(Identifier("id") -> ColumnDef(Identifier("id"), Numeric, Set.empty)),
          Set.empty))
  }

  it should "fail to parse invalid syntax" in {

    DDLParsers.apply("create foo, a table") should be('left)
  }

}
