package me.bowdon.ddldiff.ast


// Start with SQLite's grammar.
// Need plan to support multiple vendor grammars

// https://sqlite.org/datatype3.html
sealed trait SQLType extends SQL {
  override def toSQL() = toString.toLowerCase
}
case object Text extends SQLType
case object Numeric extends SQLType
case object Integer extends SQLType
case object Real extends SQLType
case object Blob extends SQLType
