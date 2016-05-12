package schema.heplers

object misc {
  def tpe[T] = null.asInstanceOf[T]
  case class TypeCaptured[T](value: T){type tpe = T}
}
