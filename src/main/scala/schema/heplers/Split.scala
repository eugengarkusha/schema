package schema.heplers

import scalaz.{Split=>_,_},Scalaz._,Id.id

//type class for adduction any monolith type to a higher kind and its argument(Id[_] and T for simple type T)
trait Split[HK,H[_],V]{
  def f(t:HK):H[V]
}
trait lo1 {
  implicit def d[HK]=new Split[HK,Id,HK]{
    def f(t:HK):Id[HK] = t
  }
}
object Split extends lo1{
  implicit def rai[H[_],V]=new Split[H[V],H,V]{
    def f(t:H[V]):H[V] = t
  }
}

