package schema

import play.api.libs.json._
import schema.heplers.{Materializer, Split}
import shapeless.labelled.{FieldType => FT, _}
import shapeless._
import shapeless.ops.hlist.Mapper

import scalaz.{Ordering => _, _}
import Scalaz._
import scala.collection.generic.CanBuildFrom


//TODO: polish this implementation
object RecordFilters {

  trait Filter[V] extends Function1[V, Option[V]]

  trait eqFilter{self:Filter.type =>
    implicit def noord[HV,H[_],V](implicit split:Split[HV,H,V], o:Optional[H], f:Functor[H],r: Reads[V]): Reads[Filter[HV]]= Reads[Filter[HV]] { jv =>
      for{
        _op <- op(jv)
        _v <- v(jv)(r)
        res <- if(_op == "EQ") JsSuccess(self[HV](hv=>o.getOrElse(f.map(split(hv))(_ == _v))(false)))
               else JsError(s"cannot parse EQ op type from ${_op}")
      } yield res
    }
  }
  trait ordFilter extends eqFilter{self:Filter.type =>

    implicit def ord[HV,H[_],V](implicit split:Split[HV,H,V], ord: Ordering[V], r: Reads[V], o:Optional[H], f:Functor[H]): Reads[Filter[HV]]= Reads[Filter[HV]]{ jv=>

     def mkFilter(op:(V)=>Boolean) = self[HV](dataVal => o.getOrElse(f.map(split(dataVal))(op(_)))(false))
     def cmp(op:(V,V)=>Boolean) = v[V](jv).map(filterVal => mkFilter(op(_, filterVal)))

      op(jv).flatMap{
        case "GT"    =>  cmp(ord.gt _)
        case "GTEQ"  =>  cmp(ord.gteq _)
        case "LT"    =>  cmp(ord.lt _)
        case "LTEQ"  =>  cmp(ord.lteq _)
        case "EQ"    =>  cmp(ord.equiv _)
        case "RANGE" => for (from<-(jv \ "from").validate[V]; to <- (jv \ "to").validate[V])
          yield mkFilter(data => ord.gteq(data, from) && ord.lteq(data, to))
        case s =>   JsError(s"cannot parse op type from $s")
      }
    }

  }
  object Filter extends ordFilter{

    def v[V](jv:JsValue)(implicit r:Reads[V]) = (jv \ "v").validate[V]
    def op(jv:JsValue) = (jv \ "op").validate[String]

    implicit def iter[H[T] <: Iterable[T], V](implicit r: Reads[H[V]]): Reads[Filter[H[V]]]= Reads[Filter[H[V]]]{jv=>
      op(jv).flatMap{
        case "OPTIONS" => v[H[V]](jv).map(opts => Filter[H[V]](data=> opts.toSeq.diff(data.toSeq).isEmpty))
        case s => JsError(s"cannot parse OPTIONS op type from $s")
      }
    }

    def apply[V](f:V=>Boolean):Filter[V] = new Filter[V]{def apply(v:V)= if(f(v))Some(v)else None}
  }


  trait loWrapWithFilters  {self:Poly1=>
    implicit def ord[K,V] = at[FT[K,V]](f=> field[K][Option[Filter[V]]](None))
  }

  object wrapWithFilters extends Poly1 with loWrapWithFilters{
    implicit def record[K,HV,H[_],V<:HList](implicit sp:Split[HV,H,V], mt: Materializer.Aux[V,V], m:Mapper[this.type,V]) = at[FT[K,HV]](f=> field[K](Option(mt.v.map(this))))
  }



  class FiltersSetup[S <: HList, F <: HList](val reads: Reads[F],val apply:ApplyFilters[F,S]){
    type filtersTpe = F
    def read(js: JsValue): JsResult[F] = reads.reads(js)
  }

  def from[S <: HList, F <: HList](schema: S)(implicit m: Mapper.Aux[wrapWithFilters.type, S,F], r: Reads[F], a: ApplyFilters[F, S]) = {
    new FiltersSetup(r, a)
  }


  trait ApplyFilters[Filters, Data]{
    def apply(s:Filters, h:Data):Option[Data]
  }

  object ApplyFilters  {

    implicit def plain[V]: ApplyFilters[Filter[V], V] = new ApplyFilters[Filter[V], V] {
      override def apply(filter: Filter[V], v: V): Option[V] = filter(v)
    }

    implicit def hlist[K, F, FTl <: HList, V, T <: HList](implicit applyFilter: ApplyFilters[F, V], filterTail: ApplyFilters[FTl, T]): ApplyFilters[FT[K, Option[F]] :: FTl, FT[K, V] :: T] =
      new ApplyFilters[FT[K, Option[F]] :: FTl, FT[K, V] :: T] {
        override def apply(filters: FT[K, Option[F]] :: FTl, data: FT[K, V] :: T): Option[FT[K, V] :: T] = {

          val filterHead = filters.head
          val dataHead =  data.head

          for {
            filteredHead <- if(filterHead.isEmpty)Some(dataHead) else filterHead.flatMap(applyFilter(_, dataHead))
            filteredTail <- filterTail(filters.tail, data.tail)
          } yield field[K](filteredHead) :: filteredTail
        }
      }

    implicit def hnil: ApplyFilters[HNil, HNil] = new ApplyFilters[HNil, HNil] {
      def apply(f: HNil, d: HNil) = Some(HNil)
    }

    implicit def monadOfHlists[F<:HList,H[_],V<:HList](implicit ht: MonadPlus[H], ie:IsEmpty[H], applyFilter: ApplyFilters[F, V]):ApplyFilters[F, H[V]] = new ApplyFilters[F, H[V]] {
      override def apply(filter: F, data: H[V]): Option[H[V]] ={
        val filtered = ht.filter(data)(v=> applyFilter(filter, v).isDefined)
        if(ie.isEmpty(filtered)) None
        else Some(filtered)
      }
    }

  }

}
