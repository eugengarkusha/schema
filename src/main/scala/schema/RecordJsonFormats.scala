package schema


import play.api.libs.json , json._
import shapeless.ops.record.{Remover, Selector, Updater}
import shapeless._, shapeless.record._, labelled._, shapeless.ops.hlist.{ToTraversable, Mapper}, syntax.singleton._

//is it generic for HLists or only for records?
object RecordJsonFormats {

  //replace with case object based impl of enum
  def enumReads[E <: Enumeration](enum: E): Reads[E#Value] = new Reads[E#Value] {
    def reads(json: JsValue): JsResult[E#Value] = json match {
      case JsString(s) => {
        try {
          JsSuccess(enum.withName(s))
        } catch {
          case _: NoSuchElementException => JsError(s"Enumeration expected of type: '${enum.getClass}', but it does not appear to contain the value: '$s'")
        }
      }
      case _ => JsError("String value expected")
    }
  }

  implicit def enumWrites[E <: Enumeration]: Writes[E#Value] = new Writes[E#Value] {
    def writes(v: E#Value): JsValue = JsString(v.toString)
  }

  //Option Reads:
  implicit def readOptOnNullUndef[V](implicit r:Reads[V]): Reads[Option[V]]= r.map(Option(_)).orElse(Reads[Option[V]](_ => JsSuccess(None)))
  //generic reads and writes for case classes
  implicit def ProductReads[P<:Product, L<:HList](implicit lg:LabelledGeneric.Aux[P,L], r: Lazy[Reads[L]]):Reads[P]= {
    r.value.map(lg.from(_))
  }
  implicit def ProductWrites[P<:Product, L<:HList](implicit lg:LabelledGeneric.Aux[P,L], w: Lazy[OWrites[L]]):OWrites[P]= {
    OWrites(c=> w.value.writes(lg.to(c)))
  }


  implicit def RecReads[K<:Symbol,V,T<:HList](implicit rv: Reads[V], w:Witness.Aux[K], rt:Lazy[Reads[T]]):Reads[FieldType[K,V]::T] = {
    //workaround for plays inconsistent parsing implementation(?):
    Reads[FieldType[K,V]::T]{v=>
      def head = (__ \ w.value.name).read[JsValue].reads(v).getOrElse(Json.obj()).validate(rv)
      def tail = rt.value.reads(v)
      head.flatMap(h=>tail.map(t=> field[K](h) :: t))
    }
  }
  implicit def NilReads: Reads[HNil] =  Reads[HNil](_ => JsSuccess(HNil))

  //HList Writes
  implicit def  RecWrites[K<:Symbol,V,T<:HList](implicit rv: Writes[V], w:Witness.Aux[K], rt:Lazy[OWrites[T]]) = OWrites[FieldType[K,V]::T]{
    l => (__ \ w.value.name).write[V].writes(l.head) ++ rt.value.writes(l.tail)
  }
  //workaround for options(play writes may have had a method to write to a path so it would be easy to implement optional writes directly)
  implicit def  RecWritesOpt[K<:Symbol,V,T<:HList](implicit rv: Writes[V], w:Witness.Aux[K], rt:Lazy[OWrites[T]]) = OWrites[FieldType[K,Option[V]]::T]{
    l => l.head.map((__ \ w.value.name).write[V].writes(_)).getOrElse(Json.obj()) ++ rt.value.writes(l.tail)
  }

  implicit def NilWrites: OWrites[HNil] =  OWrites[HNil](_ => Json.obj())

}

