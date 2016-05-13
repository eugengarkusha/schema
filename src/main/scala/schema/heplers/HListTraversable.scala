package schema.heplers

import shapeless._, shapeless.record._, labelled._, shapeless.ops.hlist.{ZipConst, ToTraversable, Mapper}, syntax.singleton._
import play.api.libs.json , json._
import shapeless.ops.record.{Updater, Selector}

import scalaz._ ,Scalaz._

//Traversable implementation for HLists(via sequence)TODO1: look for existing impls. TODO2: implement with traverse(1 pass,more efficient)
object HListTraversable {

  //Type class for sequence operation on HLIST
  trait HSeq[L,A[_]] {
    type bare
    def apply(l:L):A[bare]
  }
  object HSeq{
    type Aux[L,A[_],B]= HSeq[L,A]{type bare = B}
    implicit def HNilSeq[A[_]](implicit ap: Applicative[A]) : HSeq.Aux[HNil, A, HNil] =
      new HSeq[HNil, A]{
        type bare = HNil
        def apply(l:HNil ) = ap.point(HNil)
      }
    implicit def HCSeq[A[_], V,T<:HList, O<:HList](implicit ap:Applicative[A], evr:HSeq.Aux[T, A, O]): HSeq.Aux[A[V]::T, A, V::O] = {
      new HSeq[A[V]::T, A]{
        type bare = V::O
        def apply(l: A[V]::T)= ap.apply2(l.head, evr(l.tail))(_ :: _)
      }
    }
  }
  //todo: implement as an implicit wrapper for HLists
  object  TraversableOps{
  //test traverse more thoroughly(it may have a problems)
    def traverse[T<:HList,P<:Poly1, T1<:HList,A[_], H, H1](l:H::T,  p:P )(implicit ll:Mapper.Aux[p.type ,H::T, H1::T1], evv: H1=:=A[H], ev: HSeq[H1::T1, A]) = {
      ev(l.map(p))
    }
    def sequence[A[_],H,T<:HList](l:A[H]::T)(implicit  e: HSeq[A[H]::T, A]) = e(l)
  }

}
