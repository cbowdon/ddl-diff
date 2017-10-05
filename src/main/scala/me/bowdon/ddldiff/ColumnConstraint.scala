package me.bowdon.ddldiff

case class ColumnConstraint(name: Option[String], definition: ColumnConstraintDef)

// https://sqlite.org/syntax/column-constraint.html
sealed trait ColumnConstraintDef
case class PrimaryKey(order: Option[Order], autoIncrement: Boolean) extends ColumnConstraintDef
case object IsNotNull extends ColumnConstraintDef
case object Unique extends ColumnConstraintDef
case class Default(value: Literal) extends ColumnConstraintDef
case class Collate(collationName: String) extends ColumnConstraintDef
case class Check(expr: String) extends ColumnConstraintDef
// TODO "on delete cascade" and friends
// https://sqlite.org/syntax/foreign-key-clause.html
case class ForeignKey(table: String, columns: Seq[String]) extends ColumnConstraintDef

sealed trait Order
case object Asc extends Order
case object Desc extends Order
