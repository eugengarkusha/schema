package schema

import org.scalatest.{FlatSpec, Matchers}
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
    val hlr:hlistResultType = hl.map(process)
    val rr:recordEesultType = rec.map(process)


    rr should be  (Record(a=7,b=7,c=7))
    hlr should be (HList(7,7,7))
  }

}
