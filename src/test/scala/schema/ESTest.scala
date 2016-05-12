package schema

import java.util.Date

import play.api.libs.json.{JsObject, Json, OWrites}
import schema.EsSupport._
import schema.heplers.misc.tpe
import shapeless._
import shapeless.record._
import org.scalatest.{FlatSpec, Matchers}

class ESTest extends FlatSpec with Matchers {


  "abstract schema" should "be convertible to ES schema"  in {
    //some custom model class
    class Counter(l:Long)

    //FieldWrites for custom class
    implicit val counterEsFld = new FldWrites[Counter](new OWrites[Counter] {
      override def writes(o: Counter): JsObject = Json.obj("type" -> "long")
    })

    //schema definition(declared on value level because 1) no way to write multiline type definition by now 2)support of value level operations like updateWith used for schema derivations)
    val schemaPart1 = Record(n = tpe[List[String]], x = tpe[Counter])
    val schemaPart2 = Record(y = tpe[Date], h = tpe[Long])
    val schema = Record(a = tpe[String], b = tpe[Int], c = tpe[Boolean], x = schemaPart1, recArr = List(schemaPart2), boolArr = tpe[List[Boolean]])

    //labeling fields
    val esSchema = {
      schema.updateWith('a)(f => TstLbl(Analyzed(f)))
        .updateWith('x)(r =>
          Nested(r.updateWith('x)(Analyzed(_)))
        )
      //commented out until compilation speed is fixed for updateWith(hopefully in 2.3.1)
      //.updateWith('sl)(Nested(_))
      //.updateWith('ao)(NotAnalyzed(_))}
    }

    toEsJson(esSchema) should be(Json.parse("""{
                                 |  "properties" : {
                                 |    "a" : {
                                 |      "lbl" : "testLbl",
                                 |      "index" : "analyzed",
                                 |      "type" : "String"
                                 |    },
                                 |    "b" : {
                                 |      "type" : "int"
                                 |    },
                                 |    "c" : {
                                 |      "type" : "boolean"
                                 |    },
                                 |    "x" : {
                                 |      "index" : "nested",
                                 |      "properties" : {
                                 |        "n" : {
                                 |          "type" : "String"
                                 |        },
                                 |        "x" : {
                                 |          "index" : "analyzed",
                                 |          "type" : "long"
                                 |        }
                                 |      }
                                 |    },
                                 |    "recArr" : {
                                 |      "properties" : {
                                 |        "y" : {
                                 |          "type" : "Date"
                                 |        },
                                 |        "h" : {
                                 |          "type" : "long"
                                 |        }
                                 |      }
                                 |    },
                                 |    "boolArr" : {
                                 |      "type" : "boolean"
                                 |    }
                                 |  }
                                 |}
                                 |""".stripMargin))
  }
}
