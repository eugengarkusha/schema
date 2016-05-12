package schema.heplers

import shapeless.labelled.FieldType
import shapeless.{::, HList, HNil}

import scalaz.Applicative



//type class for HList materialization
//TODO: Materializer need fix (materialization where Out!=T is broken)
trait Materializer[T]{type Out ;val v:Out}

trait veryLowMaterializer {
  implicit def plain[T]: Materializer.Aux[T,T] = new Materializer[T]{type Out = T; val v = null.asInstanceOf[T]}
}
trait lowMaterializer extends veryLowMaterializer {

  implicit def list[T,H[_]](implicit m:Materializer.Aux[T,T], a:Applicative[H]): Materializer.Aux[H[T],H[T]] = {
    new Materializer[H[T]]{
      type Out = H[T]
      val v:Out = a.point(m.v)
    }
  }
}

object Materializer extends lowMaterializer{

  type Aux[T,O] = Materializer[T]{type Out = O}

  implicit def hconsDef1[HV,H[_],V,TT<:HList](implicit u:Unlabel[HV,H,V], d:Materializer.Aux[V,V], dt:Materializer.Aux[TT,TT]):Materializer.Aux[HV::TT,HV::TT] = {
    new Materializer[HV::TT]{
      type Out = HV::TT
      val v = u.pack(u.relabel(d.v))::dt.v
    }
  }

  implicit def HNilDef:Materializer.Aux[HNil,HNil] = new Materializer[HNil]{type Out = HNil; val v = HNil}

  def materialize[T](implicit m: Materializer[T]) = m.v

}



