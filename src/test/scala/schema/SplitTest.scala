package schema

import org.scalatest.FlatSpec
import org.scalatest.Matchers
import schema.heplers.misc.->
import schema.heplers.Split
import schema.heplers.Unlabel
import shapeless.record.Record
import shapeless.HList
import shapeless.Poly1

import scalaz.Scalaz._
import scalaz.Tag.TagOf
import scalaz._

class SplitTest extends FlatSpec with Matchers{


  "Split" should "correctly detect types1"  in {

    def plus[HK, H[_], HK1, H1[_], V,T[_]](v: HK)(v1: HK1)
    (implicit split: Split[HK, H, V], split1: Split[HK1, H1, V], f: Functor[H], f1: Foldable[H1], n: Semigroup[V]) = {
      f1.foldl(split1(v1), split(v))(b => a => f.map(b)(n.append(a, _)))
    }
    plus(1)(2)             should be(3)
    plus(List(1, 2, 3))(2) should be(List(3, 4, 5))
    plus(2)(List(1, 2, 3)) should be(8)
    plus(List(1, 2, 3))(List(4, 5, 6)) should be(List(16, 17, 18))
    plus(Option(5L))(2L)   should be(Some(7L))
    plus(Option(5L))(List(2L, 3L)) should be(Some(10L))
    plus(List(2L, 3L))(Option(5L)) should be(List(7L, 8L))
    plus(Option.empty[Long])(List(7L, 8L)) should be(None)
    plus(List(7L, 8L))(Option.empty[Long])should be(List(7L, 8L))
  }
  "Unlabel" should  "allow mapping over HList and Records preserving structure" in {
    val hl = HList(1,true,"3")
    val rec = Record(a=1,b=true,c="3")

    type hlistResultType = HList.`Int,Int,Int`.T
    type recordEesultType = Record.`'a->Int,'b->Int,'c->Int`.T


    object process extends Poly1{
      implicit def ct[HV,V](implicit u:Unlabel[HV,V]) = at[HV](hv=>u.relabel(7))
    }
    //Use typed equals check insted (find appropriate method in scalatest)
    val hres:hlistResultType = hl.map(process)
    val rres:recordEesultType = rec.map(process)


    rres should be  (Record(a=7,b=7,c=7))
    hres should be (HList(7,7,7))

  }

  "lifted monofuntion" should "preserve structure of HList and Records" in{

    type hlistResultType = HList.`Int,Int,Int`.T
    type recordEesultType = Record.`'a->Int,'b->Int,'c->Int`.T

    val rr = Record(a="",b="one",c="four")
    val hh = HList("","one","four")

    object size extends ->[String, Int](_.size)
    object size1 extends shapeless.PolyDefns.->[String, Int](_.size)

    import shapeless._
    import record._
    val rres:recordEesultType =  rr.map(size)
    val rres1: recordEesultType =  rr.mapValues(size1)
    val hres:hlistResultType =  hh.map(size)

    rres should be  (Record(a=0,b=3,c=4))
    hres should be (HList(0,3,4))

  }



}
