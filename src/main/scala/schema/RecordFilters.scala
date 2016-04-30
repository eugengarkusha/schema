package schema

import play.api.libs.json._
import schema.heplers.{Materializer, Split}
import shapeless.labelled.{FieldType => FT, _}
import shapeless._
import shapeless.ops.hlist.Mapper
import play.api.libs.functional.syntax._

import scalaz.{Ordering => _, _}
import scalaz.Isomorphism.<~>
import Scalaz._


//TODO: polish this implementation(wrapWithFilters and ApplyFilters are so necessary?)
//TODO: add reporting of which filter has failed
object RecordFilters {

  //TODO:it sould be defined somewhere in scalaz
  //this is needed to be in scope in client code
  implicit val isoListIterable = new <~>[List, List]{
    override val to: ~>[List, List] = NaturalTransformation.refl[List]
    override val from: ~>[List, List] = to
  }

  trait Filter[V] extends Function1[V, Option[V]]

 //TODO: avoid duplication of nonordering ops!!!!
  trait eqFilter{self:Filter.type =>

    implicit def noord[HV,H[_],V](implicit split:Split[HV,H,V], o:Optional[H], f:Functor[H],r: Reads[V]): Reads[Filter[HV]]= Reads[Filter[HV]] { jv =>
      for{
        _op <- op(jv)
        _v <- v(jv)(r).map(Set(_)).orElse(v[Set[V]](jv))
        res <- if(_op == "EQ") JsSuccess(Filter.bool[HV](hv=>o.getOrElse(f.map(split(hv))(_v.contains(_)))(false)))
               else JsError(s"cannot parse EQ op type from ${_op}")
      } yield res
    }
  }
  trait ordFilter extends eqFilter{self:Filter.type =>


    implicit def ord[HV,H[_],V](implicit split:Split[HV,H,V], ord: Ordering[V], r: Reads[V], o:Optional[H], f:Functor[H]): Reads[Filter[HV]]= Reads[Filter[HV]]{ jv=>

     def mkFilter(op:(V)=>Boolean) = Filter.bool[HV](dataVal => o.getOrElse(f.map(split(dataVal))(op(_)))(false))
     def cmp(op:(V,V)=>Boolean) = v[V](jv).map(filterVal => mkFilter(op(_, filterVal)))

      op(jv).flatMap{
        case "GT"    =>  cmp(ord.gt _)
        case "GTEQ"  =>  cmp(ord.gteq _)
        case "LT"    =>  cmp(ord.lt _)
        case "LTEQ"  =>  cmp(ord.lteq _)
        case "EQ"=>  v[V](jv).map(Set(_)).orElse(v[Set[V]](jv)).map(ops => mkFilter(ops.contains(_)))
        case "RANGE" => for (from<-(jv \ "from").validate[V]; to <- (jv \ "to").validate[V])
          yield mkFilter(data => ord.gteq(data, from) && ord.lteq(data, to))
        case s =>   JsError(s"cannot parse op type from $s")
      }
    }

  }
  object Filter extends ordFilter{

    def apply[V](f:V => Option[V]) =new Filter[V]{def apply(v:V)= f(v)}
    def bool[V](f:V=>Boolean):Filter[V] = new Filter[V]{def apply(v:V)= if(f(v))Some(v)else None}
    def v[V](jv:JsValue)(implicit r:Reads[V]) = (jv \ "v").validate[V]
    def op(jv:JsValue) = (jv \ "op").validate[String]
    def elFilterRaw(jv: JsValue) = (jv \ "elFilter").validate[JsValue]
    //Isomorphism to list is to rigid conatraint here? Client has to define isomorphisms from all list-like structures he euses
    implicit def group[H[_], V](implicit iso: H<~>List, r: Reads[H[V]], rf: Reads[Filter[V]]): Reads[Filter[H[V]]]= Reads[Filter[H[V]]] { jv =>

      val elFlt = elFilterRaw(jv)
      val _op = op(jv)

      if(elFlt.isError && _op.isError)JsError("neither element filter or group filter is defined")
      else {

        val valueFilter: JsResult[Filter[H[V]]] = elFlt match {
          case JsSuccess(flt, _) => rf.reads(flt).map { f =>
            Filter[H[V]] { data =>
              val filtered = iso.to(data).filter(f(_).isDefined)
              if (filtered.isEmpty) None else Some(iso.from(filtered))
            }
          }
          case _: JsError => JsSuccess(Filter[H[V]](Some(_)))
        }


        val groupFilter: JsResult[Filter[H[V]]] = {
          _op match {
            case JsSuccess("OPTIONS", _) => v[H[V]](jv).map(opts => Filter.bool[H[V]](data => iso.to(opts).diff(iso.to(data)).isEmpty))
            case JsSuccess("SIZE", _) => v[Int](jv).map(sz => Filter.bool[H[V]](data => iso.to(data).size >= sz))
            case JsSuccess(s, _) => JsError(s"cannot parse OPTIONS op type from $s")
            case _: JsError => JsSuccess(Filter[H[V]](Some(_)))
          }
        }

        valueFilter and groupFilter apply ((fv, pf) => Filter[H[V]](fv.andThen(_.flatMap(pf(_)))))
      }
   }

    implicit def recordFilter[V<:HList, F<:HList](implicit m: Mapper.Aux[wrapWithFilters.type,V,F], r:Reads[F], af:ApplyFilters[F,V]):Reads[Filter[V]] =
      r.map(f=> Filter[V](af(f,_)))
  }


  object wrapWithFilters extends Poly1 {
    implicit def ord[K,V] = at[FT[K,V]](f=> field[K][Option[Filter[V]]](None))
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

  }

}
