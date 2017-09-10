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

  it should "parse primary key column constraints" in {

    DDLParser.apply("create table foo (id numeric primary key asc autoincrement);") shouldEqual
    Right(
      TableDef(
        "foo",
        Seq(ColumnDef("id", Numeric, Seq(PrimaryKey(Some(Asc), true)))),
        Seq.empty))
  }

  it should "parse not null column constraints" in {

    DDLParser.apply("create table foo (id numeric not null);") shouldEqual
    Right(
      TableDef(
        "foo",
        Seq(ColumnDef("id", Numeric, Seq(IsNotNull))),
        Seq.empty))
  }

  it should "parse unique column constraints" in {

    DDLParser.apply("create table foo (id numeric unique);") shouldEqual
    Right(
      TableDef(
        "foo",
        Seq(ColumnDef("id", Numeric, Seq(Unique))),
        Seq.empty))
  }

  it should "parse collate column constraints" in {

    DDLParser.apply("create table foo (name text collate binary);") shouldEqual
    Right(
      TableDef(
        "foo",
        Seq(ColumnDef("name", Text, Seq(Collate("binary")))),
        Seq.empty))
  }

  it should "parse default column constraints (numeric)" in {

    DDLParser.apply("create table foo (id numeric default 0);") shouldEqual
    Right(
      TableDef(
        "foo",
        Seq(ColumnDef("id", Numeric, Seq(Default(NumericLiteral(0))))),
        Seq.empty))
  }

  it should "parse default column constraints (signed numeric)" in {

    DDLParser.apply("create table foo (id numeric default -1);") shouldEqual
    Right(
      TableDef(
        "foo",
        Seq(ColumnDef("id", Numeric, Seq(Default(SignedNumber(NumericLiteral(1), Minus))))),
        Seq.empty))
  }

  it should "parse default column constraints (string literal)" in {

    DDLParser.apply("create table foo (name text default 'Bob');") shouldEqual
    Right(
      TableDef(
        "foo",
        Seq(ColumnDef("name", Text, Seq(Default(StringLiteral("Bob"))))),
        Seq.empty))
  }

  it should "parse default column constraints (null)" in {

    DDLParser.apply("create table foo (name text default null);") shouldEqual
    Right(
      TableDef(
        "foo",
        Seq(ColumnDef("name", Text, Seq(Default(Null)))),
        Seq.empty))
  }

  it should "parse default column constraints (current time)" in {

    DDLParser.apply("create table foo (name text default current_time);") shouldEqual
    Right(
      TableDef(
        "foo",
        Seq(ColumnDef("name", Text, Seq(Default(CurrentTime)))),
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
