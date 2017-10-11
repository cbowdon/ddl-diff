package me.bowdon.ddldiff.ast

import me.bowdon.ddldiff._


case class Identifier(value: String) extends SQL {
  override def toSQL() = value
}
