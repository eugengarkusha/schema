package schema


import java.util
import java.util.ArrayList
import shapeless._
import shapeless.{HList, |∨|, Witness, Poly1}
import shapeless.labelled.FieldType
import scala.collection.JavaConverters._
import weka.core.{Instances, Attribute}
import scala.collection.breakOut


//in order to transform abstract schema to weka schema user has to
//1)for field types which have no WekaAttribute instances provided by lib:  provide thece implicit instances
//2)for enum fields :  replace type of Enumeration#Value with ENumeration object
//3)for embedded records - wrap with Relational
object WekaSupport {

  case class Relational[L<:HList](l: L, instancesName: String, capacity: Int)

  //type class for mapping values to corresponding Weka attributes
  trait WekaAttribute[T]{
    def attr(name: String, v:T): Attribute
  }

  object WekaAttribute{

    implicit def numbers[T:Numeric] = new WekaAttribute[T] {def attr(name:String, v:T) = new Attribute(name)}

    implicit def strAndBool[T:(String|∨|Boolean)#λ] = new WekaAttribute[T] {def attr(name:String, v:T) = new Attribute(name)}

    implicit def enum[E<:Enumeration] = new WekaAttribute[E] {
      def attr(name: String, e: E) = {
        val list = new ArrayList[String]
        list.addAll(e.values.map(_.toString)(breakOut).asJava)
        new Attribute(name, list)
      }
    }

    implicit def relational[L<:HList](implicit tw: ToWekaAttrs[L]) = new WekaAttribute[Relational[L]] {
      def attr(name: String, rel: Relational[L]) = {
        val fieldList = new util.ArrayList[Attribute]
        fieldList.addAll(tw.attrs(rel.l).asJava)
        new Attribute(name, new Instances(rel.instancesName, fieldList, rel.capacity))
      }
    }
  }

  implicit class ToWekaAttresWrapper[L<:HList](l: L)(implicit tw: ToWekaAttrs[L]){
    def wekaAttrs= tw.attrs(l)
  }

  //type class for conversing HList to List of Weka Attributes
  trait ToWekaAttrs[L<:HList]{
    def attrs(l:L): List[Attribute]
  }

  implicit def HListToWekaAttrs[HK<:Symbol,HV,T<:HList](implicit w: Witness.Aux[HK], wa: WekaAttribute[HV], tw: ToWekaAttrs[T]) = {
    new ToWekaAttrs[FieldType[HK,HV]::T]{
      def attrs(l: FieldType[HK,HV]::T) = wa.attr(w.value.name, l.head.asInstanceOf[HV]) :: tw.attrs(l.tail)
    }
  }

  implicit  val HNilToWekaAttrs = new ToWekaAttrs[HNil] {def attrs(n:HNil) = Nil}

}
