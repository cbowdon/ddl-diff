package me.bowdon

import org.scalatest._
import org.scalatest.Matchers._


object ColumnConstraintParsersImpl extends ColumnConstraintParsers {
  def apply(input: String): Either[String, ColumnConstraint] = {
    parseAll(columnConstraint, input) match {
      case Success(result, _) => Right(result)
      case failure: NoSuccess => Left(failure.msg)
    }
  }
}

class ColumnConstraintParsersSpec extends FlatSpec with Matchers {

  "ColumnConstraints" should "parse primary key column constraints" in {

    ColumnConstraintParsersImpl.apply("primary key asc autoincrement") shouldEqual Right(ColumnConstraint(None, PrimaryKey(Some(Asc), true)))
  }

  it should "parse not null column constraints" in {

    ColumnConstraintParsersImpl.apply("not null") shouldEqual Right(ColumnConstraint(None, IsNotNull))
  }

  it should "parse unique column constraints" in {

    ColumnConstraintParsersImpl.apply("unique") shouldEqual Right(ColumnConstraint(None, Unique))
  }

  it should "parse collate column constraints" in {

    ColumnConstraintParsersImpl.apply("collate binary") shouldEqual Right(ColumnConstraint(None, Collate("binary")))
  }

  it should "parse default column constraints (numeric)" in {

    ColumnConstraintParsersImpl.apply("default 0") shouldEqual Right(ColumnConstraint(None, Default(NumericLiteral(0))))
  }

  it should "parse default column constraints (signed numeric)" in {

    ColumnConstraintParsersImpl.apply("default -1") shouldEqual Right(ColumnConstraint(None, Default(SignedNumber(NumericLiteral(1), Minus))))
  }

  it should "parse default column constraints (string literal)" in {

    ColumnConstraintParsersImpl.apply("default 'Bob'") shouldEqual Right(ColumnConstraint(None, Default(StringLiteral("Bob"))))
  }

  it should "parse default column constraints (null)" in {

    ColumnConstraintParsersImpl.apply("default null") shouldEqual Right(ColumnConstraint(None, Default(Null)))
  }

  it should "parse default column constraints (current time)" in {

    ColumnConstraintParsersImpl.apply("default current_time") shouldEqual Right(ColumnConstraint(None, Default(CurrentTime)))
  }

  it should "parse check column constraints" in {

    ColumnConstraintParsersImpl.apply("check (something > nothing)") shouldEqual Right(ColumnConstraint(None, Check("something > nothing")))
  }

  it should "parse named column constraints" in {

    ColumnConstraintParsersImpl.apply("constraint it_aint_null not null") shouldEqual Right(ColumnConstraint(Some("it_aint_null"), IsNotNull))
  }
}
