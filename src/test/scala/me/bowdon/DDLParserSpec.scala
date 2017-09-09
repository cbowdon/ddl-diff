package me.bowdon

import org.scalatest._
import org.scalatest.Matchers._

class DDLParserSpec extends FlatSpec with Matchers {
    val parser = new DDLParser

  "The DDLParser class" should "parse a simple create" in {

    parser.apply("create table foo (id number);") shouldEqual Right(Seq(Create, Table("foo"), Columns(Column("id", Number))))
  }

  it should "parse multiple columns" in {

    parser.apply("create table foo (id number, name text);") shouldEqual Right(Seq(Create, Table("foo"), Columns(Column("id", Number), Column("name", Text))))
  }

  it should "not care if people are shouting" in {

    parser.apply("CREATE TABLE foo (id NUMBER);") shouldEqual Right(Seq(Create, Table("foo"), Columns(Column("id", Number))))
  }

  it should "not mind a missing terminator" in {

    parser.apply("CREATE TABLE foo (id NUMBER)") shouldEqual Right(Seq(Create, Table("foo"), Columns(Column("id", Number))))
  }

  it should "fail to parse invalid syntax" in {

    parser.apply("create foo, a table") should be ('left)
  }

}
