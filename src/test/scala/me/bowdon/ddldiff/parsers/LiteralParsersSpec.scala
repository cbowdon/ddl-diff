package me.bowdon.ddldiff.parsers

import org.scalatest._
import org.scalatest.Matchers._
import me.bowdon.ddldiff.ast._

object LiteralParsersImpl extends LiteralParsers {
  // Just to exercise the parsers in a test-friendly way
  def apply(input: String): Either[ParseError, Literal] = {
    parseAll(literalValue, input) match {
      case Success(result, _) => Right(result)
      case failure: NoSuccess => Left(new ParseError(failure.msg))
    }
  }
}

class LiteralParsersSpec extends FlatSpec with Matchers {

  "LiteralParsers" should "parse numerics" in {
    LiteralParsersImpl.apply("3.142") shouldEqual Right(NumericLiteral(3.142))
  }

  "LiteralParsers" should "parse strings" in {
    LiteralParsersImpl.apply("'Bob'") shouldEqual Right(StringLiteral("Bob"))
  }

  "LiteralParsers" should "parse nulls" in {
    LiteralParsersImpl.apply("null") shouldEqual Right(Null)
  }

  "LiteralParsers" should "parse current timestamp" in {
    LiteralParsersImpl.apply("current_timestamp") shouldEqual Right(CurrentTimestamp)
  }

  "LiteralParsers" should "parse current time" in {
    LiteralParsersImpl.apply("current_time") shouldEqual Right(CurrentTime)
  }

  "LiteralParsers" should "parse current date" in {
    LiteralParsersImpl.apply("current_date") shouldEqual Right(CurrentDate)
  }

}
