package schema.heplers

import shapeless.labelled._

import scalaz.{Split => _, _}
import Scalaz._
import Id.Id


//this type class serves a purpose of abstraction over label
//it may be used for unifying processing of HLists and Records
trait Unlabel[Labeld,V] {
  type Label[_]
  def repack(hv:Label[V]):Labeld
  def apply(v:Labeld): V
  def relabel[X](v:X):Label[X]
}

trait loUnlabel{
  implicit def plain[V]: Unlabel.Aux[Id[V], Id, V] = new Unlabel[Id[V], V]{
    type Label[X]= Id[X]
    def apply(v:V)= v
    def relabel[X](h:X):X = h
    def repack(v:V)= v
  }
}
object Unlabel extends loUnlabel{

type Aux[Labeld,L[_],V]= Unlabel[Labeld,V]{type Label[X]=L[X]}

  implicit def fieldType[K,V] :Unlabel.Aux[FieldType[K, V],({type Lbl[X]=FieldType[K,X]})#Lbl, V]  = {
    new Unlabel[FieldType[K, V], V] {
      type Label[X]= FieldType[K,X]
      def apply(v:FieldType[K, V]):V = v
      def relabel[X](h: X): FieldType[K,X] = field[K](h)
      def repack(hv:FieldType[K, V]):FieldType[K, V] = hv
    }
  }
}

//this type class does adduction any monolith type to a higher kind and its argument(Id[_] and T for simple type T)
//unlike shapeless.Unpack1(that explicitly aimed to match HK[_] types), this class has a symantic of unifying processing of HK and plain types
//method pack is facilitates preserving of exact type shape
//method apply is used in combination with typclasses for H[_]

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

