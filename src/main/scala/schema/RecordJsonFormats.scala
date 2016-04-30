package schema

import play.api.libs.json._
import shapeless._, labelled._

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


  trait PathAwareReads[T]{
    def by(p: JsPath):Reads[T]
  }
  trait loPathAwareReads {
    implicit def noOpt[T](implicit r:Reads[T]):PathAwareReads[T] = new PathAwareReads[T] {
      override def by(p: JsPath): Reads[T] = p.read[T]
    }
  }
  object PathAwareReads extends loPathAwareReads{
    implicit def opt[T](implicit r:Reads[T]):PathAwareReads[Option[T]] = new PathAwareReads[Option[T]] {
      override def by(p: JsPath): Reads[Option[T]] = p.readNullable[T]
    }
  }

  //generic reads and writes for case classes
  implicit def productReads[P<:Product, L<:HList](implicit lg:LabelledGeneric.Aux[P,L], r: Lazy[Reads[L]]):Reads[P]= {
    r.value.map(lg.from(_))
  }
  implicit def productWrites[P<:Product, L<:HList](implicit lg:LabelledGeneric.Aux[P,L], w: Lazy[OWrites[L]]):OWrites[P]= {
    OWrites(c=> w.value.writes(lg.to(c)))
  }


  implicit def recReads[K<:Symbol,V,T<:HList](implicit rv: PathAwareReads[V], w:Witness.Aux[K], rt:Lazy[Reads[T]]):Reads[FieldType[K,V]::T] = {
    Reads[FieldType[K,V]::T]{v=>
      def head = rv.by(__ \ w.value.name).reads(v)
      def tail = rt.value.reads(v)
      head.flatMap(h=>tail.map(t=> field[K](h) :: t))
    }
  }
  implicit def nilReads: Reads[HNil] =  Reads[HNil](_ => JsSuccess(HNil))

  //HList Writes
  implicit def  recWrites[K<:Symbol,V,T<:HList](implicit rv: Writes[V], w:Witness.Aux[K], rt:Lazy[OWrites[T]]) = OWrites[FieldType[K,V]::T]{
    l => (__ \ w.value.name).write[V].writes(l.head) ++ rt.value.writes(l.tail)
  }
  //workaround for options(play writes may have had a method to write to a path so it would be easy to implement optional writes directly)
  implicit def  recWritesOpt[K<:Symbol,V,T<:HList](implicit rv: Writes[V], w:Witness.Aux[K], rt:Lazy[OWrites[T]]) = OWrites[FieldType[K,Option[V]]::T]{
    l => l.head.map((__ \ w.value.name).write[V].writes(_)).getOrElse(Json.obj()) ++ rt.value.writes(l.tail)
  }

  implicit def nilWrites: OWrites[HNil] =  OWrites[HNil](_ => Json.obj())

}

