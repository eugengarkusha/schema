package schema.heplers

import shapeless.{::, HList, HNil}


//type class for HList materialization
trait Materializer[T]{type Out ;val v:Out}

trait lowMaterializer {
  implicit def default[T]: Materializer.Aux[T,T] = new Materializer[T]{type Out = T; val v = null.asInstanceOf[T]}
}

object Materializer extends lowMaterializer{

  type Aux[T,O] = Materializer[T]{type Out = O}

  implicit def hconsDef1[H,HO, TT<:HList,O<:HList](implicit d:Materializer.Aux[H,HO], dt:Materializer.Aux[TT,O]):Materializer.Aux[H::TT,HO::O] = {
    new Materializer[H::TT]{
      type Out = HO::O
      val v = d.v::dt.v
    }
  }

  implicit def HNilDef:Materializer.Aux[HNil,HNil] = new Materializer[HNil]{type Out = HNil; val v = HNil}

}


////user provided materializers
//implicit val matStr = new Materializer[String]{
//  type Out = Function1[String,Boolean]
//  val v:Out = _.startsWith("a")
//}
//implicit val matInt = new Materializer[Int]{
//  type Out =  Function1[Int,Boolean]
//  val v:Out = _ > 1
//}
//implicit def matCaeClass[T,H](implicit  g:Generic.Aux[T,H],m: Materializer[H]) = m.asInstanceOf[Materializer[T]]
////test case class
//case class A(a: Int, b:String)
////result
//// /Materializer.build[A].value
//
//implicitly [Materializer[A]].v

