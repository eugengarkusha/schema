package schema

import org.scalatest.{FlatSpec, Matchers}
import schema.heplers.misc._
import shapeless._
import shapeless.record._
import RecordFilters._
import RecordJsonFormats._
import play.api.libs.json.{Json, Reads}

import scalaz._
import scalaz.Isomorphism.Iso2
import scalaz.NaturalTransformation._
import Scalaz._

class RecordFiltersTest  extends FlatSpec with Matchers{

  //Schema
  type inner = Record.`'d->Int, 'e->Boolean`.T
  type inner2 = Record.`'x->Long, 'f->Boolean`.T
  type schema = Record.`'ao->Option[Double],'a->List[Int],'b->String,'c->inner, 'cl->List[inner2]`.T

  //Data chunk
  val innerData: inner = Record(d = 2,e = true)
  val inner2Data: inner2 = Record(x = 1L ,f = false)
  val data: schema =  Record(ao= Some(5D), a =  List(1,2,3), b = "bar", c = innerData, cl = List(inner2Data, inner2Data.updated('x, 2L)))

  //settings(received over the wire or smth)
  val filtersConfig ="""{
  |  "ao":{
  |     "op":"LT",
  |     "v" : 6
  |  },
  |  "a" : {
  |    "op" : "OPTIONS",
  |    "v" : [ 1, 3 ],
  |    "elFilter":{
  |       "op":"GT",
  |       "v": 0
  |     }
  |  },
  |  "b" : {
  |    "op" : "EQ",
  |    "v" : ["bar","car", "blah"]
  |  },
  |  "c" : {
  |    "d" : {
  |      "op" : "GTEQ",
  |      "v" : 2
  |    },
  |    "e" : {
  |      "op" : "EQ",
  |      "v" : true
  |    }
  |  },
  |  "cl" : {
  |    "op" : "SIZE",
  |    "v" : 1,
  |    "elFilter":{
  |      "x" : {
  |        "op" : "GT",
  |        "v" : 0
  |      },
  |      "f" : {
  |        "op" : "EQ",
  |        "v" : false
  |      }
  |    }
  |  }
  |}
  |""".stripMargin




   val filterReads =  implicitly[Reads[Filter[schema]]]
   val flt = filterReads.reads(Json.parse(filtersConfig)).get


  "data that fully passes filter" should "be unchanged" in {
    flt(data) should be (Some(data))
  }
//
  "plain fields" should  "be filtrable" in {
    flt(data.updated('b, "foo")) should be (None)
  }
//
  "optional field with Some" should "be filtered as plain field" in {
    flt(data.updated('ao, Option(7D))) should be (None)
    val updData = data.updated('ao, Option(0D))
    flt(updData) should be (Some(updData))
  }
//
  "optional field with NONE" should "not pass the filter" in {
    flt(data.updated('ao, None:Option[Double])) should be (None)
  }
  "multiple choice equals" should "pass if any option from the list matches" in {
    val updData1=data.updated('b, "car")
    flt(updData1) should be (Some(updData1))
    val updData2=data.updated('b, "blah")
    flt(updData2) should be (Some(updData2))
    flt(data.updated('b, "baz")) should be (None)
  }
//
  "List field" should "pass filter if all entries in filter are contained in this list" in {
    flt(data.updated('a, List(1,4))) should be (None)
    val updData = data.updated('a, List(1,3,7,9))
    flt(updData) should be (Some(updData))
  }
//
  "Fields with nested records" should "be filtered out if one of the nested records fields is not passing filter" in {
    flt( data.updateWith('c)(_.updated('d,0))) should be (None)
    val updData = data.updateWith('c)(_.updated('d,10))
    flt(updData) should be (Some(updData))
  }

  "Fields with list of nested records" should "contain the list with records that are passing the filter. If list is empty,the enclosing recoed considered not passing a filter" in {
    flt(data.updateWith('cl){l=>l.head.updated('x,-1L)::l.tail}) should be (Some(data.updateWith('cl)(_.tail)))
    flt(data.updateWith('cl){_(1).updated('f,true)::Nil}) should be (None)
  }

  "filters covering just a part of data structure" should "be applicable" in {
    val f ="""
      |{
      |  "ao" : {
      |     "op":"LT",
      |     "v" : 10
      |  },
      |  "a" : {
      |    "op" : "OPTIONS",
      |    "v" : [ 2, 3 ]
      |  },
      |  "cl" : {
      |    "elFilter":{
      |      "x" : {
      |        "op" : "GT",
      |        "v" : 0
      |      }
      |    }
      |  }
      |}""".stripMargin

    filterReads.reads(Json.parse(f)).get.apply(data) should be (Some(data))
    val f2 ="""
             |{
             | "c" : {
             |    "d" : {
             |      "op" : "LT",
             |      "v" : 200
             |    }
             |  },
             |  "cl" : {
             |    "op" : "SIZE",
             |    "v" : 1,
             |    "elFilter":{
             |      "x" : {
             |        "op" : "GT",
             |        "v" : 0
             |      },
             |      "f" : {
             |        "op" : "EQ",
             |        "v" : false
             |      }
             |    }
             |  }
             |} """.stripMargin

    filterReads.reads(Json.parse(f2)).get.apply(data) should be (Some(data))

  }
  "group filters" should "be applied after element filters" in {
    val f ="""
             |{
             |  "a" : {
             |    "op" : "OPTIONS",
             |    "v" : [ 2, 3 ],
             |    "elFilter":{
             |       "op":"GT",
             |       "v": 2
             |     }
             |  }
             |}""".stripMargin

    filterReads.reads(Json.parse(f)).get.apply(data) should be (None)

    val f1 ="""
             |{
             |  "a" : {
             |    "op" : "OPTIONS",
             |    "v" : [3],
             |    "elFilter":{
             |       "op":"GT",
             |       "v": 2
             |     }
             |  }
             |}""".stripMargin

    filterReads.reads(Json.parse(f1)).get.apply(data) should be (Some(data.updated('a,List(3))))

    val f3 ="""
              |{
              |  "cl" : {
              |    "op" : "SIZE",
              |    "v" : 2,
              |    "elFilter":{
              |      "x" : {
              |        "op" : "GT",
              |        "v" : 1
              |      }
              |    }
              |  }
              |} """.stripMargin

    filterReads.reads(Json.parse(f3)).get.apply(data) should be (None)
    val f4 ="""
              |{
              |  "cl" : {
              |    "op" : "SIZE",
              |    "v" : 1,
              |    "elFilter":{
              |      "x" : {
              |        "op" : "GT",
              |        "v" : 1
              |      }
              |    }
              |  }
              |} """.stripMargin

    filterReads.reads(Json.parse(f4)).get.apply(data) should be (Some(data.updateWith('cl)(_.tail)))
  }

//
////   TODO: add filters deserializer tests
////

}
