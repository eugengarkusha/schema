package schema
import org.scalatest.{FlatSpec, Matchers}
import schema.heplers.Materializer
import schema.heplers.Materializer._
import shapeless._
import shapeless.record._

import scalaz._
import Scalaz._

//TODO: Materializer need fix (materialization where Out!=T is broken)
class MaterializerTest extends FlatSpec with Matchers{


  "Hlists with nested applicatives"should  "be materializable" in {
    type h = HList.`Option[Int],Boolean`.T
    materialize[h] should be(None :: null:: HNil)
    materialize[Option[h]] should be(None)
  }

  "Records with nested applicatives" should  "be materializable" in {
    type r = Record.`'a->List[Option[Int]],'b-> Boolean`.T
    materialize[List[r]] should be( List(List(None) :: null :: HNil))
  }


//  ////user provided materializers
//  implicit val matStr = new Materializer[String]{
//    type Out = Function1[String,Boolean]
//    val v:Out = _.startsWith("a")
//  }
//  implicit val matInt = new Materializer[Int]{
//    type Out =  Function1[Int,Boolean]
//    val v:Out = _ > 1
//  }
////  implicit def matCaeClass[T,H](implicit  g:Generic.Aux[T,H],m: Materializer[H]) = m//.asInstanceOf[Materializer[T]]
//  //test case class
//  type r =Record.`'a-> Int, 'b->String`.T
//  type r1 =Record.`'a-> Int Function1 Boolean , 'b->String Function1 Boolean `.T
//
//  //result
//  println(Materializer.materialize[r])
//
//  println(implicitly[Materializer.Aux[r,r1]].v)
}
