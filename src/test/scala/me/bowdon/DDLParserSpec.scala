package me.bowdon

import org.scalatest._
import org.scalatest.Matchers._

class DDLParserSpec extends FlatSpec with Matchers {

  "The DDLParser class" should "parse a simple create" in {

    DDLParser.apply("create table foo (id numeric);") shouldEqual
    Right(
      TableDef(
        "foo",
        Seq(ColumnDef("id", Numeric, Seq.empty)),
        Seq.empty))
  }

  it should "parse multiple columns" in {

    DDLParser.apply("create table foo (id numeric, name text);") shouldEqual
    Right(
      TableDef(
        "foo",
        Seq(
          ColumnDef("id", Numeric, Seq.empty),
          ColumnDef("name", Text, Seq.empty)),
        Seq.empty))
  }

  it should "parse single column constraints" in {

    DDLParser.apply("create table foo (id numeric primary key asc autoincrement);") shouldEqual
    Right(
      TableDef(
        "foo",
        Seq(ColumnDef("id", Numeric, Seq(PrimaryKey(Some(Asc), true)))),
        Seq.empty))
  }

  it should "parse multiple column constraints" in {

    DDLParser.apply("create table foo (id numeric primary key, name text unique not null collate nocase, size numeric default 0);") shouldEqual
    Right(
      TableDef(
        "foo",
        Seq(
          ColumnDef("id", Numeric, Seq(PrimaryKey(None, false))),
          ColumnDef("name", Text, Seq(Unique, IsNotNull, Collate("nocase"))),
          ColumnDef("size", Numeric, Seq(Default(NumericLiteral(0))))),
        Seq.empty))
  }

  it should "ignore modifiers like 'if not exists'" in {

    DDLParser.apply("create table if not exists foo (id numeric);") shouldEqual
    Right(
      TableDef(
        "foo",
        Seq(ColumnDef("id", Numeric, Seq.empty)),
        Seq.empty))
  }

  it should "ignore modifiers like 'temp'" in {

    DDLParser.apply("create temp table if not exists foo (id numeric);") shouldEqual
    Right(
      TableDef(
        "foo",
        Seq(ColumnDef("id", Numeric, Seq.empty)),
        Seq.empty))
  }

  it should "not care if people are shouting" in {

    DDLParser.apply("CREATE TABLE foo (id NUMERIC);") shouldEqual
    Right(
      TableDef(
        "foo",
        Seq(ColumnDef("id", Numeric, Seq.empty)),
        Seq.empty))
  }

  it should "not care about messy whitespace" in {

    DDLParser.apply("CREATE     TABLE  foo    ( id\t\tNUMERIC );") shouldEqual
    Right(
      TableDef(
        "foo",
        Seq(ColumnDef("id", Numeric, Seq.empty)),
        Seq.empty))
  }

  it should "not mind a missing terminator" in {

    DDLParser.apply("CREATE TABLE foo (id NUMERIC)") shouldEqual
    Right(
      TableDef(
        "foo",
        Seq(ColumnDef("id", Numeric, Seq.empty)),
        Seq.empty))
  }

  it should "fail to parse invalid syntax" in {

    DDLParser.apply("create foo, a table") should be ('left)
  }

}
