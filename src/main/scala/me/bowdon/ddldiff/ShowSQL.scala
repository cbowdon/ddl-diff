package me.bowdon.ddldiff

import me.bowdon.ddldiff.ast.SQL

object ShowSQL {
  // Allow us to write: sql"create table $name ($column);"
  // where $name is a String and $column is a ColumnDef

  // Originally this was a type class, but since the sql"" literal could
  // be called with a Seq[Object] we needed to do the pattern matching on
  // a base type anyway, so ended up just implementing a toSQL method.

  implicit class SQLStringHelper(val sc: StringContext) extends AnyRef {
    def sql(args: Any*): String = {
      val sqlStrings = args.map(arg => arg match {
        case x: SQL => x.toSQL
        case x: Any => x.toString
      })
      sc.s(sqlStrings:_*)
    }
  }
}
