package schema

import java.util

import WekaSupport._
import schema.heplers.misc.tpe
import shapeless.record.Record
import shapeless.record._
import weka.core.{Attribute, Instances}
import scala.collection.JavaConverters._
import org.scalatest.{FlatSpec, Matchers}

class WekaTest extends FlatSpec with Matchers{



  "Abstract schama" should "be convertible to Weka schema" in {

    object Tst extends Enumeration{
      val aval, bval, cval = Value
    }

    val schema = Record(n=tpe[String] , x=tpe[Int] , r=Record(v="",y=true),enum = tpe[Tst.Value])


    //in order to transform abstract schema to weka schema user has to
    //1)for field types which have no WekaAttribute instances provided by lib:  provide thece implicit instances
    //2)for enum fields :  replace type of Enumeration#Value with ENumeration object
    //3)for embedded records - wrap with Relational
    schema.updateWith('enum)(_ => Tst).updateWith('r)(Relational(_, "instancesName", 10)).wekaAttrs should be{

      val inner ={
        val l = new util.ArrayList[Attribute](2)
        l.add(new Attribute("v")); l.add(new Attribute("y"))
        l
      }

      Seq(new Attribute("n"),
          new Attribute("x"),
          new Attribute("r", new Instances("instancesName",inner ,10)),
          new Attribute("enum", Tst.values.map(_.toString).toList.asJava)
      )
    }
  }

}
