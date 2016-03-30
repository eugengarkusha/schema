package schema


import shapeless._
import record._
import labelled._
import ops.hlist.{Mapper}
import syntax.singleton._
import play.api.libs.json
import json._
import schema.heplers._
import HListTraversable._
import org.scalatest.{FlatSpec, Matchers}

import scalaz.{syntax => _, _}
import Scalaz._

class HListTraversableTest extends FlatSpec with Matchers {



 "sequence over HList" should "behave similar to sequence over lists" in {
   TraversableOps.sequence(Option(1)::Option(true)::Option("2") :: None.asInstanceOf[Option[Nothing]]::  HNil) should be (None)
   TraversableOps.sequence(Option(1)::Option(true)::Option("2") ::  HNil) should be (Some(1 :: true :: "2" :: HNil))
 }

  "traverse over HList" should "behave similar to sequence over lists" in {
    object toOp extends Poly1 {implicit def toOp[T] = at[T](Option(_))}
    TraversableOps.traverse(HList(1, true, "2"), toOp) should be (Some(HList(1, true, "2")))
    TraversableOps.traverse(HList(1, null, "2"), toOp) should be (None)
  }

  "Reads[Hlist] implementation implemented terms of map and sequence" should "behave as usual Reads" in {

    //scalaz Applicative impl for json Reads
    implicit object ReaadsApplicative extends Applicative[Reads]{
      override def point[A](a: => A): Reads[A] = Reads[A](_=>JsSuccess(a))
      override def ap[A, B](fa: => Reads[A])(f: => Reads[(A) => B]): Reads[B] = f.flatMap(ff=>fa.map(ff))
    }

    object mkReads extends Poly1 {
      implicit def mkr[K<:Symbol,V](implicit w:Witness.Aux[K], r:Reads[V]) = at[FieldType[K,V]](_=> (__ \ w.value.name).read[V].map(field[K](_)))
    }

    //record reads implicit implemented with HSeq
    implicit def HR[L<:HList, O<:HList](implicit m: Mapper.Aux[mkReads.type,L,O], seq: HSeq.Aux[O, Reads, L], mat: Materializer.Aux[L,L]): Reads[L] = {
      seq(mat.v.map(mkReads))
    }

    val ordJs = Json.obj("n"->"N", "x"-> 17, "r"-> Json.obj("v"->"V", "y"->true))
    val ord = Record(n = "N" , x = 17, r = Record(v = "V", y = true))
    type schemaPart = Record.`'v -> String,'y ->Boolean`.T
    type schema  = Record.`'n-> String, 'x-> Int, 'r->schemaPart`.T
    implicitly[Reads[schema]].reads(ordJs) should be (JsSuccess(ord))


  }


}
