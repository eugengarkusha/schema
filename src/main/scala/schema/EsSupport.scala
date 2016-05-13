package schema

import play.api.libs.json._
import shapeless._
import shapeless.labelled.{FieldType => FT}
import play.api.libs.json
import json.Json.toJsFieldJsValueWrapper
import schema.heplers.Materializer


object EsSupport{

  trait FldOption[T]{val t:T}

  case class Analyzed[T](t: T)(implicit ev: T <:!< HList) extends FldOption[T]
  case class NotAnalyzed[T](t: T)(implicit ev: T <:!< HList) extends FldOption[T]
  //http://milressabin.com/blog/2011/06/09/scala-union-types-curry-howard/
  case class Nested[T:(HList|∨|Seq[HList])#λ](t: T) extends FldOption[T]
  case class TstLbl[T](t: T) extends FldOption[T]

  implicit def HListWrites[K<:Symbol,V,T<:HList](implicit rv: FldWrites[V], w:Witness.Aux[K], rt:Lazy[OWrites[T]])= OWrites[FT[K,V]::T] { l =>
    RecordJsonFormats.recWrites(rv.t,w,rt).writes(l)
  }

  implicit def hnilWrites = RecordJsonFormats.nilWrites

  def toEsJson[T<:HList](v:T)(implicit w: FldWrites[T]) = w.t.writes(v)

}

class FldWrites[T](val t: OWrites[T])

object FldWrites{

  import EsSupport._

  private def lblFlds[A[_]<:FldOption[_],T](lbl:String, fldName:String="index")(implicit w: FldWrites[T], ev:A[T]<:<FldOption[T]) = new FldWrites(OWrites[A[T]] { a =>
    Json.obj(fldName -> lbl) ++ w.t.writes(ev(a).t)
  })

  implicit def analyzedWr[T](implicit w: FldWrites[T]):FldWrites[Analyzed[T]] = lblFlds[Analyzed,T]("analyzed")
  implicit def notAnalyzedWr[T](implicit w: FldWrites[T]):FldWrites[NotAnalyzed[T]] = lblFlds[NotAnalyzed,T]("not_analyzed")
  implicit def nestedWr[T](implicit w: FldWrites[T]):FldWrites[Nested[T]] =  lblFlds[Nested,T]("nested")
  //dummy label just for demo
  implicit def tstLblWr[T](implicit w: FldWrites[T]):FldWrites[TstLbl[T]] =  lblFlds[TstLbl,T]("testLbl","lbl")

  //TODO: remove "bogus" default writes and provide the real mappings from scala types to es types!!
  implicit def dfltWrites[T](implicit w: Manifest[T]) = new FldWrites(OWrites[T]{ _=>
    Json.obj("type" -> w.runtimeClass.getSimpleName)
  })

  implicit def SeqWr[S[_] <: Seq[_], T](implicit w: FldWrites[T], mr: Materializer[T]):FldWrites[S[T]] = new FldWrites(OWrites[S[T]] { _ =>
    w.t.writes(mr.v)
  })

  implicit def HListFieldWrites [H<:HList](implicit rt:Lazy[OWrites[H]]):FldWrites[H] = new FldWrites(OWrites[H]{ l=>
    Json.obj("properties" -> rt.value.writes(l))
  })

}





