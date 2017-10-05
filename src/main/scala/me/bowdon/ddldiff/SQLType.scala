package me.bowdon.ddldiff

// Start with SQLite's grammar.
// Need plan to support multiple vendor grammars

// https://sqlite.org/datatype3.html
sealed trait SQLType
case object Text extends SQLType
case object Numeric extends SQLType
case object Integer extends SQLType
case object Real extends SQLType
case object Blob extends SQLType
