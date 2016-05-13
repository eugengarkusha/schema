package schema

import org.scalatest.{FlatSpec, Matchers}
import schema.heplers.misc.->
import schema.heplers.{Split, Unlabel}
import shapeless.{HList, Poly1}
import shapeless.record.Record

import scalaz._
import Scalaz._

class SplitTest extends FlatSpec with Matchers{


  "Split" should "correctly detect types"  in {

    def plus[HK,H[_],V](v:HK)(v1:V)(implicit split: Split[HK,H,V], f: Functor[H], n: Numeric[V]) = f.map(split(v))(n.plus(_, v1))

    plus(1)(2)             should be(3)
    plus(List(1, 2, 3))(2) should be(List(3, 4, 5))
    plus(Option(5D))(2D)   should be(Some(7D))
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

    val rres:recordEesultType =  rr.map(size)
    val hres:hlistResultType =  hh.map(size)

    rres should be  (Record(a=0,b=3,c=4))
    hres should be (HList(0,3,4))

  }



}
