package schema

import org.scalatest.{FlatSpec, Matchers}
import schema.heplers.misc._
import shapeless._
import shapeless.record._
import RecordFilters._
import RecordJsonFormats._
import play.api.libs.json.Json
import scalaz.{Ordering => _, _}
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
  |     "op":"LTEQ",
  |     "v" : 6
  |  },
  |  "a" : {
  |    "op" : "OPTIONS",
  |    "v" : [ 1, 3 ]
  |  },
  |  "b" : {
  |    "op" : "EQ",
  |    "v" : "bar"
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
  |    "x" : {
  |      "op" : "GT",
  |      "v" : 0
  |    },
  |    "f" : {
  |      "op" : "EQ",
  |      "v" : false
  |    }
  |  }
  |}
  |""".stripMargin


   //FiltersSetup
   val filters =  RecordFilters.from(tpe[schema])


   val filtersValues = filters.read(Json.parse(filtersConfig)).get

  "data that fully passes filter" should "be unchanged" in {
    filters.apply(filtersValues, data) should be (Some(data))
  }

  "plain fields" should  "be filtrable" in {
    filters.apply(filtersValues, data.updated('b, "foo")) should be (None)
  }

  "optional field with Some" should "be filtered as plain field" in {
    filters.apply(filtersValues, data.updated('ao, Option(7D))) should be (None)
    val updData = data.updated('ao, Option(0D))
    filters.apply(filtersValues, updData) should be (Some(updData))
  }

  "optional field with NONE" should "not pass the filter" in {
    filters.apply(filtersValues, data.updated('ao, None:Option[Double])) should be (None)
  }

  "List field" should "pass filter if all entries in filter are contained in this list" in {
    filters.apply(filtersValues, data.updated('a, List(1,4))) should be (None)
    val updData = data.updated('a, List(1,3,7,9))
    filters.apply(filtersValues, updData) should be (Some(updData))
  }

  "Fields with nested records" should "be filtered out if one of the nested records fields is not passing filter" in {
    filters.apply(filtersValues, data.updateWith('c)(_.updated('d,0))) should be (None)
    val updData = data.updateWith('c)(_.updated('d,10))
    filters.apply(filtersValues, updData) should be (Some(updData))
  }

  "Fields with list of nested records" should "contain the list with records that are passing the filter. If list is empty,the enclosing recoed considered not passing a filter" in {
    filters.apply(filtersValues, data.updateWith('cl){l=>l.head.updated('x,-1L)::l.tail}) should be (Some(data.updateWith('cl)(_.tail)))
    filters.apply(filtersValues, data.updateWith('cl){_(1).updated('f,true)::Nil}) should be (None)
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
      |    "x" : {
      |      "op" : "GT",
      |      "v" : 0
      |    }
      |   }
      |}""".stripMargin

    filters.apply(filters.read(Json.parse(f)).get , data) should be (Some(data))
  }

  // TODO: add filters deserializer tests


}
