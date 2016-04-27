package schema.heplers

import shapeless.labelled.FieldType

import scalaz.{Split => _, _}
import Scalaz._
import Id.id

//type class for adduction any monolith type to a higher kind and its argument(Id[_] and T for simple type T)
trait Split[HK,H[_],V] extends Function1[HK,H[V]]
trait lo1 {
  implicit def _id[HK]=new Split[HK,Id,HK]{
    def apply(t:HK):Id[HK] = t
  }

}
object Split extends lo1{
  implicit def hk[H[_],V]=new Split[H[V],H,V]{
    def apply(t:H[V]):H[V] = t
  }
}


//type class for abstracting over field labeling
//used to facilitate generic processing of Records and HLists
trait Unlabel[H,V]extends Function1[H,V]{
  def relabel(v:V):H = v.asInstanceOf[H]
}

trait loUnlabel{
  implicit def plain[H] = new Unlabel[H,H]{
    def apply(h:H):H =h
  }
}
object Unlabel extends loUnlabel{
  implicit def hl[K,V] = new Unlabel[FieldType[K,V], V]{
    def apply(h:FieldType[K,V]):V = h
  }

}
