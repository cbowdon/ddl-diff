package me.bowdon.ddldiff.ast

import me.bowdon.ddldiff.ShowSQL
import ShowSQL._

case class ColumnConstraint(name: Option[Identifier], definition: ColumnConstraintDef) extends SQL {
  override def toSQL() = {
      this match {
        case ColumnConstraint(None, ccDef) => sql"$ccDef"
        case ColumnConstraint(Some(name), ccDef) => sql"constraint $name $ccDef"
      }
  }
}


// https://sqlite.org/syntax/column-constraint.html
sealed trait ColumnConstraintDef extends SQL {
  override def toSQL() = {
    this match {
        case Unique => "unique"
        case IsNotNull => "not null"
        case Default(literal) => sql"default $literal"
        case Collate(collation) => sql"collate $collation"
        case Check(expr) => s"check ($expr)"
        case PrimaryKey(orderOpt, autoIncrement) => {
          val order = orderOpt match {
            case None => ""
            case Some(x) => sql"$x"
          }
          val autoInc = if (autoIncrement) "autoincrement" else ""
          Seq("primary key", order, autoInc).filter(x => x != "").mkString(" ")
        }
        case ForeignKey(table, cols) => sql"references $table(${cols.map(_.toSQL).mkString(", ")})"
      }
  }
}
case class PrimaryKey(order: Option[Order], autoIncrement: Boolean) extends ColumnConstraintDef
case object IsNotNull extends ColumnConstraintDef
case object Unique extends ColumnConstraintDef
case class Default(value: Literal) extends ColumnConstraintDef
case class Collate(collationName: Identifier) extends ColumnConstraintDef
case class Check(expr: String) extends ColumnConstraintDef
// TODO "on delete cascade" and friends
// https://sqlite.org/syntax/foreign-key-clause.html
case class ForeignKey(table: Identifier, columns: Seq[Identifier]) extends ColumnConstraintDef
