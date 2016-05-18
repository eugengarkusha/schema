package schema.heplers

import shapeless.labelled.FieldType
import shapeless.{::, HList, HNil}

import scalaz.{Applicative, Id}



//type class for HList materialization
class Materializer[T](val v: T)

trait l0 {
  implicit def plain[T]: Materializer[T] = new Materializer(null.asInstanceOf[T])
}
trait l1{
  implicit def list[H[_],T](implicit a:Applicative[H], m:Materializer[T]): Materializer[H[T]] = {
    new Materializer[H[T]](a.point(m.v))
  }
}

object Materializer extends l1 with l0{

  implicit def hconsDef1[HV,V,TT<:HList](implicit u:Unlabel[HV,V], d:Materializer[V], dt:Materializer[TT]):Materializer[HV::TT] = {
    //TODO: cannot find H[V]<:<HV
    new Materializer[HV::TT](u.relabel(d.v).asInstanceOf[HV]::dt.v)
  }

  implicit def HNilDef:Materializer[HNil] = new Materializer(HNil)

  implicit def opt[T]: Materializer[Option[T]] = new Materializer[Option[T]](None)

  def materialize[T](implicit m: Materializer[T]) = m.v

}



