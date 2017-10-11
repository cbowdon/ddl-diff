package me.bowdon.ddldiff.ast

/**
  * Base for all SQL types in AST
  */
trait SQL {
  def toSQL(): String
}
