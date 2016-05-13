package schema.heplers

import shapeless.labelled._

import scalaz.{Split => _, _}
import Scalaz._
import Id.Id

//this type class does relabeling any type with captured kind(Id or FieldType)
trait Unlabel[Labeld,V] {
  type Lbl[_]
  def apply(v:Labeld): V
  def relabel[X](v:X):Lbl[X]
}

trait loUnlabel{
  implicit def plain[V] = new Unlabel[Id[V],V]{
    type Lbl[X]= Id[X]
    def apply(v:V)= v
    override def relabel[X](h:X):X = h
  }
}
object Unlabel extends loUnlabel{

type Aux[Labeld,L[_],V]= Unlabel[Labeld,V]{type Lbl[X]=L[X]}

  implicit def fieldType[K,V] :Unlabel.Aux[FieldType[K, V],({type Lbl[X]=FieldType[K,X]})#Lbl, V]  = {
    new Unlabel[FieldType[K, V], V] {
      type Lbl[X]= FieldType[K,X]
      def apply(v:FieldType[K, V]):V = v
      def relabel[X](h: X): FieldType[K,X] = field[K](h)
    }
  }
}

//this type class does adduction any monolith type to a higher kind and its argument(Id[_] and T for simple type T)
//it acts almost like shapeless.Unpack provides additional operations
//method pack is facilitates preserving of exact type shape
//method apply is used in combination with typclasses for H[_]
//TODO:Remove this class, and use shapeless.Unpack in conjucntion with type equality evidences(or identity isomorphism)
 trait Split[HV,H[_],V] {
  def apply(hv:HV):H[V]
  def pack(hv:H[V]):HV
}

trait loSplit{
  implicit def plain[V] = new Split[Id[V],Id,V]{
    def apply(hv:V)=hv
    def pack(hv:V)=hv
  }
}
object Split extends loSplit{
  implicit def applicative[H[_],V]=new Split[H[V],H,V]{
    def apply(t:H[V]):H[V] = t
    def pack(t:H[V]):H[V] = t
  }

}

