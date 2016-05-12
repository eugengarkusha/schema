package schema.heplers

import shapeless.labelled._

import scalaz.{Split => _, _}
import Scalaz._
import Id.Id

//this type class does:
//1) adduction any monolith type to a higher kind and its argument(Id[_] and T for simple type T)  - apply method
//2) relabeling any type with captured kind  - relabel mthod
//method pack is provided to facilitate preserving of exact type shape
//instance for FieldType is provided OOB
//TODO: need to have separate Split typeclass supporting only split operation(HV->H[V]) and separate Unlabel type class only for FieldTypes
//TODO: having separate Unlabel typeclass will enable refactoring of shapeless polyfnctions to preserve labels of Records

trait Unlabel[HV,H[_],V] {
  def relabel[X](v:X):H[X]
  def apply(hv:HV):H[V]
  def pack(hv:H[V]):HV
}

trait loUnlabel{
  implicit def plain[V] = new Unlabel[Id[V],Id,V]{
    override def relabel[X](h:X):X = h
    def apply(hv:V)=hv
    def pack(hv:V)=hv
  }
}
object Unlabel extends loUnlabel{

  implicit def fieldType[K,V] = {
    type FT[V]=FieldType[K,V]
    new Unlabel[FieldType[K, V], FT, V] {
      def relabel[X](h: X): FieldType[K, X] = field[K](h)
      def apply(hv:FieldType[K, V])= hv
      def pack(hv:FieldType[K, V])= hv
    }
  }

  implicit def applicative[H[_],V](implicit p: Applicative[H])=new Unlabel[H[V],H,V]{
    def relabel[X](v:X):H[X]= p.pure(v)
    def apply(t:H[V]):H[V] = t
    def pack(t:H[V]):H[V] = t
  }

}
