package schema

import org.scalatest.{FlatSpec, Matchers}
import play.api.libs.json._
import schema.RecordJsonFormats._
import shapeless.record.Record

class JsonTest extends FlatSpec with Matchers{

  "records" should "be serializable/deserializeble"  in{

    object Tst extends Enumeration{
      val aval, bval, cval = Value
    }
    implicit val er = enumReads(Tst)

    val ordJs = Json.obj("n"->"N", "x"-> 17, "r"-> Json.obj("v"->"V", "y"->true), "enum"-> "cval")
    type schemaPart = Record.`'v -> String,'y ->Boolean`.T
    type schema  = Record.`'n-> String, 'x-> Int, 'r->schemaPart,'enum -> Tst.Value`.T
    val ord = Record(n = "N" , x = 17, r = Record(v = "V", y = true),enum = Tst.cval)

    val format = implicitly[Format[schema]]
    format.reads(ordJs).asOpt should be (Some(ord))
    format.writes(ord) should be (ordJs)
  }

  "recursive case classes" should "be serializable/deserializeble" in{
    case class C (a:String, b:Int,rec:Option[C])
    val js = Json.obj("a"->"A","b"->3,"rec"->Json.obj("a"->"inner","b"->2))
    val cclass =  C("A", 3, Some(C("inner", 2, None)))

    val format = implicitly[Format[C]]
    format.reads(js).asOpt should  be(Some(cclass))
    format.writes(cclass) should be (js)
  }

  "arbitratry nested case class structure" should "be serializable/deserializable" in {

    case class I1(i:Int,b:Boolean)
    case class I2(i2:Option[Int],b2:Option[Boolean],ii:I1, ii2:Option[I2])
    case class O (x: String, i2: I2, i1:I1)

    val instance = O("X",I2(Some(2),Some(true),I1(11,true),Some(I2(None, Some(false),I1(9,false),None))),I1(10,true))

    val js = Json.parse("""{
                          |  "x" : "X",
                          |  "i2" : {
                          |    "i2" : 2,
                          |    "b2" : true,
                          |    "ii" : {
                          |      "i" : 11,
                          |      "b" : true
                          |    },
                          |    "ii2" : {
                          |      "b2" : false,
                          |      "ii" : {
                          |        "i" : 9,
                          |        "b" : false
                          |      }
                          |    }
                          |  },
                          |  "i1" : {
                          |    "i" : 10,
                          |    "b" : true
                          |  }
                          |}
                          |""".stripMargin)

    //note that no additional explicit play reads/writes are defined
    val ff = implicitly[Format[O]]
    ff.writes(instance) should be (js)
    ff.reads(js).asOpt should be (Some(instance))
  }




}
