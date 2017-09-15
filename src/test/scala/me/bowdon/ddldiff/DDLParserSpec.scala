package me.bowdon.ddldiff

import org.scalatest._
import org.scalatest.Matchers._

class DDLParserSpec extends FlatSpec with Matchers {

  "The DDLParser class" should "parse a simple create" in {

    DDLParser.apply("create table foo (id numeric);") shouldEqual
    Right(
      TableDef(
        "foo",
        Map("id" -> ColumnDef("id", Numeric, Set.empty)),
        Set.empty))
  }

  it should "parse multiple columns" in {

    DDLParser.apply("create table foo (id numeric, name text);") shouldEqual
    Right(
      TableDef(
        "foo",
        Map(
          "id" -> ColumnDef("id", Numeric, Set.empty),
          "name" -> ColumnDef("name", Text, Set.empty)),
        Set.empty))
  }

  it should "not parse multiple columns with trailing comma" in {

    DDLParser.apply("create table foo (id numeric, name text,);") should be ('left)
  }

  it should "parse single column constraints" in {

    DDLParser.apply("create table foo (id numeric primary key asc autoincrement);") shouldEqual
    Right(
      TableDef(
        "foo",
        Map("id" -> ColumnDef("id", Numeric, Set(ColumnConstraint(None, PrimaryKey(Some(Asc), true))))),
        Set.empty))
  }

  it should "parse multiple column constraints" in {

    DDLParser.apply("""
create table foo (
  id numeric primary key
, name text constraint foo_unique_name_constraint unique not null collate nocase
, size numeric default 0
);""") shouldEqual
    Right(
      TableDef(
        "foo",
        Map(
          "id" -> ColumnDef("id", Numeric, Set(ColumnConstraint(None, PrimaryKey(None, false)))),
          "name" -> ColumnDef("name", Text,
            Set(
              ColumnConstraint(Some("foo_unique_name_constraint"), Unique),
              ColumnConstraint(None, IsNotNull),
              ColumnConstraint(None, Collate("nocase")))),
          "size" -> ColumnDef("size", Numeric, Set(ColumnConstraint(None, Default(NumericLiteral(0)))))),
        Set.empty))
  }

  it should "ignore modifiers like 'if not exists'" in {

    DDLParser.apply("create table if not exists foo (id numeric);") shouldEqual
    Right(
      TableDef(
        "foo",
        Map("id" -> ColumnDef("id", Numeric, Set.empty)),
        Set.empty))
  }

  it should "ignore modifiers like 'temp'" in {

    DDLParser.apply("create temp table if not exists foo (id numeric);") shouldEqual
    Right(
      TableDef(
        "foo",
        Map("id" -> ColumnDef("id", Numeric, Set.empty)),
        Set.empty))
  }

  it should "not care if people are shouting" in {

    DDLParser.apply("CREATE TABLE foo (id NUMERIC);") shouldEqual
    Right(
      TableDef(
        "foo",
        Map("id" -> ColumnDef("id", Numeric, Set.empty)),
        Set.empty))
  }

  it should "not care about messy whitespace" in {

    DDLParser.apply("CREATE     TABLE  foo    ( id\t\tNUMERIC );") shouldEqual
    Right(
      TableDef(
        "foo",
        Map("id" -> ColumnDef("id", Numeric, Set.empty)),
        Set.empty))
  }

  it should "not mind a missing terminator" in {

    DDLParser.apply("CREATE TABLE foo (id NUMERIC)") shouldEqual
    Right(
      TableDef(
        "foo",
        Map("id" -> ColumnDef("id", Numeric, Set.empty)),
        Set.empty))
  }

  it should "fail to parse invalid syntax" in {

    DDLParser.apply("create foo, a table") should be ('left)
  }

}
