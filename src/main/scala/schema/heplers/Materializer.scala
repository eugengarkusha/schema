package schema.heplers

import shapeless.labelled.FieldType
import shapeless.{::, HList, HNil}

import scalaz.{Applicative, Id}



//type class for HList materialization
//TODO: Materializer need fix (materialization where Out!=T is broken)
class Materializer[T](val v: T)

trait lowMaterializer {
  implicit def plain[T]: Materializer[T] = new Materializer(null.asInstanceOf[T])
}

object Materializer extends lowMaterializer{

  implicit def hconsDef1[HV,V,TT<:HList](implicit u:Unlabel[HV,V], d:Materializer[V], dt:Materializer[TT]):Materializer[HV::TT] = {
    //TODO: cannot find H[V]<:<HV
    new Materializer[HV::TT](u.relabel(d.v).asInstanceOf[HV]::dt.v)
  }

  implicit def HNilDef:Materializer[HNil] = new Materializer(HNil)

  implicit def list[H[_],T](implicit a:Applicative[H], m:Materializer[T]): Materializer[H[T]] = {
    new Materializer[H[T]](a.point(m.v))
  }

  def materialize[T](implicit m: Materializer[T]) = m.v

}



