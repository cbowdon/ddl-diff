package me.bowdon.ddldiff.ast


sealed trait Order extends SQL {
  override def toSQL() = {
      this match {
        case Asc => "asc"
        case Desc => "desc"
      }
  }
}
case object Asc extends Order
case object Desc extends Order
