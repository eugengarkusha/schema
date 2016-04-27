package schema

import org.scalatest.{FlatSpec, Matchers}
import schema.heplers.Split
import scalaz._, Scalaz._

class SplitTest extends FlatSpec with Matchers{


  "Split" should "correctly detect types"  in {

    def plus[HK,H[_],V](v:HK)(v1:V)(implicit split: Split[HK,H,V], f: Functor[H], n: Numeric[V]) = f.map(split(v))(n.plus(_, v1))

    plus(1)(2)             should be(3)
    plus(List(1, 2, 3))(2) should be(List(3, 4, 5))
    plus(Option(5D))(2D)   should be(Some(7D))
  }

}
