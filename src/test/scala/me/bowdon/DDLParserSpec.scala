package me.bowdon

import org.scalatest._
import org.scalatest.Matchers._

class DDLParserSpec extends FlatSpec with Matchers {
  val parser = new DDLParser

  "The DDLParser class" should "parse a simple create" in {

    parser.apply("create table foo (id number);") shouldEqual
    Right(
      TableDef(
        "foo",
        Seq(ColumnDef("id", Number, Seq.empty)),
        Seq.empty))
  }

  it should "parse multiple columns" in {

    parser.apply("create table foo (id number, name text);") shouldEqual
    Right(
      TableDef(
        "foo",
        Seq(
          ColumnDef("id", Number, Seq.empty),
          ColumnDef("name", Text, Seq.empty)),
        Seq.empty))
  }

  it should "ignore modifiers like 'if not exists'" in {

    parser.apply("create table if not exists foo (id number);") shouldEqual
    Right(
      TableDef(
        "foo",
        Seq(ColumnDef("id", Number, Seq.empty)),
        Seq.empty))
  }

  it should "ignore modifiers like 'temp'" in {

    parser.apply("create temp table if not exists foo (id number);") shouldEqual
    Right(
      TableDef(
        "foo",
        Seq(ColumnDef("id", Number, Seq.empty)),
        Seq.empty))
  }

  it should "not care if people are shouting" in {

    parser.apply("CREATE TABLE foo (id NUMBER);") shouldEqual
    Right(
      TableDef(
        "foo",
        Seq(ColumnDef("id", Number, Seq.empty)),
        Seq.empty))
  }

  it should "not mind a missing terminator" in {

    parser.apply("CREATE TABLE foo (id NUMBER)") shouldEqual
    Right(
      TableDef(
        "foo",
        Seq(ColumnDef("id", Number, Seq.empty)),
        Seq.empty))
  }

  it should "fail to parse invalid syntax" in {

    parser.apply("create foo, a table") should be ('left)
  }

}
