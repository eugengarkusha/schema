package schema.heplers

import shapeless.Poly1

object misc {
  def tpe[T] = null.asInstanceOf[T]
  case class TypeCaptured[T](value: T){type tpe = T}

  class ->[T, R](f : T => R) extends Poly1 {
    implicit def subT[HK,V <: T](implicit u:Unlabel[HK,V]) = at[HK](hk=>u.relabel(f(u(hk))))
  }
}
