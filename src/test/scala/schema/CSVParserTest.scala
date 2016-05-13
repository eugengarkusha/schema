package schema

import shapeless._
import shapeless.record._
import schema.heplers.misc.tpe
import shapeless.Nat._
import schema.CSVParser._
import org.scalatest.{FlatSpec, Matchers}

import scala.util.Success


class CSVParserTest extends FlatSpec with Matchers {

  //How does abstract schema is supposed to be encoded:
  //a)on type level:
  //--pros: consistency,...
  //--cons: multiline definitions are not supported(which blocks the possibility to define "big" shemas on type level); no possibility of direct transormations
  //b)on value level:
  //--pros: multiline definitions; value level transformations(using library methods)
  //--cons: value level encoding is not consistent with "abstract" concept(because value level is concrete); ugly "tpe" null based workarounds

  //choosing value level approach:
  val schema = Record(a = tpe[Int], b = tpe[String], c = Record(d = tpe[String],e = tpe[Boolean]), f = tpe[String])

  val testRow = Record(a = 12, b = "blah", c = Record(d = "foo", e = true), f = "bar")

  val tokenizer = new Tokenizer {def apply(s:String) = s.split(",").toVector}

  "default CSV parser" should "be deriveable from the abstract schema " in {
    val p1 = schema.defaultParser.build(tokenizer)
    p1.parse("12,blah,foo,true,bar") should be (Success(testRow))
  }

  "derived default field parsers" should "be customizable " in {
    val p2 = schema.setupParser(s =>
      s.updated('a, (s: String) => s.toInt + 2).
        updateWith('b)(_.andThen(_ + "!!!")).index
    ).build(tokenizer)
    p2.parse("12,blah,foo,true,bar") should be(Success(testRow.updateWith('a)(_ + 2)
                                                          .updateWith('b)(_ + "!!!")))
  }

  "derived default field parsers" should "be mappable to arbitraty index in CSV string" in {
    val p3 = schema.setupParser(s =>
      s.updateWith('a)(_ -> Nat(3)).
        updateWith('b)(_ -> Nat(0)).
        updateWith('c)(_.updateWith('d)(_ -> Nat(1)).updateWith('e)(_ -> Nat(4))).
        updateWith('f)(_ -> Nat(2))
    ).build(tokenizer)
    p3.parse("blah,foo,bar,12,true") should be (Success(testRow))
  }

  "derived default field parsers" should "be customizable and indexable simultaneously" in {
    val p4 = schema.setupParser(s =>
      s.updateWith('a)(_ => ((s: String) => {
        val v = s.toInt; if (v > 11) 100 else 0
      }) -> _3).
        updateWith('b)(_.andThen(_ + "111") -> _0).
        updateWith('c)(_.updateWith('d)(_ -> _1).updateWith('e)(_ -> _4)).
        updateWith('f)(_ => ((s: String) => s.take(2)) -> _2)
    ).build(tokenizer)

    p4.parse("blah,foo,bar,12,true") should be(Success(testRow.updateWith('a)(v=> if (v > 11) 100 else 0)
                                                          .updateWith('b)(_ + "111")
                                                          .updateWith('f)(_.take(2))))
  }
}
