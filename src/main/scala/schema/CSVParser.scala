package schema

import org.joda.time.format.{DateTimeFormat, ISODateTimeFormat}
import shapeless.ops.hlist.{Mapper, ZipConst}
import shapeless._
import scala.util.Try
import shapeless.labelled.{FieldType => FT, _}
import shapeless.ops.nat.{LT, ToInt}


trait CSVParser[Schema<:HList]{
  def parse(s:String): Try[Schema]
}

object CSVParser {
  trait Tokenizer extends Function1[String, Vector[String]]

  trait CSVFieldParser[T] {
    val func: Function1[String, T]
  }

  object CSVFieldParser {

    //if mkParser is not return type annotated then (probably due to compiler bug) very wierd behaviour on Record.updated is observed:
    //fldParsers = schema.map(getFldParserByType)
    //fldParsers.updated('companyId, CSVFieldParser.intParser) -> creates a duplicate FieldType['companyId, CSVFieldParser.intParser](noramally it happens only if provided value type is not the same)
    //subsequent identical calls to update method work as expected i.e. replace 'companyId fld value with new value
    private def mkParser[T](f:String=> T): CSVFieldParser[T] = new CSVFieldParser[T]{ val func = f}

    implicit val defaultIntParser = mkParser(_.toInt)
    implicit val defaultDoubleParser = mkParser(_.toDouble)
    implicit val defaultBooleanParser = mkParser(_.trim.toLowerCase.matches("true|y|yes|1"))
    implicit val defaultDateTimeParser = mkParser(ISODateTimeFormat.localDateParser.parseDateTime(_))
    implicit val defaultStringParser = new CSVFieldParser[String]{
      val func = (s:String) => if(s.nonEmpty)s else throw new Exception("Empty string detected in String field")
    }
    implicit def defaultOptionParser[T](implicit fp: CSVFieldParser[T]) = new CSVFieldParser[Option[T]]{
      val func = (s:String) => Try(fp.func(s)).toOption
    }

  }


  private  object applyFldParser extends Poly1 {
    implicit def caseT[K, R, N<:Nat](implicit ti: ToInt[N]) = at[(FT[K, (Function1[String, R],N)], Vector[String])]{
      case(f, row)=> field[K](f._1(row(ti())))
    }
    implicit def caseT1[K, V <: HList, O <: HList](implicit zc:ZipConst.Aux[Vector[String], V, O], m:Mapper[this.type ,O]) =
      at[(FT[K, V], Vector[String])]{
        case(l, row)=> field[K](l.asInstanceOf[V].zipConst(row)(zc).map(this))
      }
  }

  trait lowGetFldParserByType {this: getFldParserByType.type =>
    implicit def caseT1[K, V](implicit parser: CSVFieldParser[V]) = at[FT[K, V]](_ => field[K](parser.func))
  }
  object getFldParserByType extends Poly1 with lowGetFldParserByType {
    implicit def caseT[K, V<:HList](implicit parser: Mapper[this.type, V]) = at[FT[K, V]](v => field[K](v.asInstanceOf[V].map(this)))
  }

  def dateTimeParser(pattern: String) =  DateTimeFormat.forPattern(pattern).parseDateTime _

  ///type class for automatic Record deep indexing
  trait RecordIndexer[L <: HList, SI <: Nat] extends DepFn1[L] {
    type EI <: Nat
    type Out <: HList
  }


  trait LoRecordIndexer {

    implicit def HLI[K,H, T <: HList, Si <: Nat](implicit hli: RecordIndexer[T, Succ[Si]]) = new RecordIndexer[FT[K,H] :: T, Si] {
      type Out = FT[K,(H, Si)] :: hli.Out
      type EI = hli.EI
      def apply(l: FT[K,H] :: T): Out = field[K](l.head -> null.asInstanceOf[Si]) :: hli(l.tail)
    }
  }

  object RecordIndexer extends LoRecordIndexer {
    type Aux[L <: HList, Si <: Nat, Ei <: Nat, O <: HList] = RecordIndexer[L, Si] {type Out = O; type EI = Ei}

    implicit def HLI2[K, H <: HList, T <: HList, EH <: Nat, Si <: Nat, O <: HList, EE <: Nat, OO <: HList]
    (implicit hlih: RecordIndexer.Aux[H, Si, EH, O], hlit: RecordIndexer.Aux[T, EH, EE, OO]):
      RecordIndexer.Aux[FT[K, H] :: T, Si, EE, FT[K, O] :: OO] =
         new RecordIndexer[FT[K, H] :: T, Si] {
          type Out = FT[K, O] :: OO
          type EI = EE

          def apply(l: FT[K, H] :: T): FT[K, O] :: OO = field[K](hlih(l.head)) :: hlit(l.tail)
      }

    implicit def HNI[Si <: Nat]: RecordIndexer.Aux[HNil, Si, Si, HNil] = new RecordIndexer[HNil, Si] {
      type Out = HNil
      type EI = Si

      def apply(l: HNil): Out = HNil
    }
  }

  //type class for witnessing the presence of correct record indexing
  //TODO: Implement macro based NAT(for faster operations)
  @annotation.implicitNotFound("Provided record is either unindexed or indexed incorrectly. Indexed evidence not found for ${L}")
  class Indexed[-L<:HList,N<:Nat]
  private val dfltInd  = new Indexed[HNil, _0]

  object Indexed {
    implicit def ri [H,T<:HList,I<:Nat,MaxInd<:Nat](implicit ev: LT[I,MaxInd], i:Indexed[T,MaxInd]): Indexed[(H,I)::T, MaxInd] = {
      i.asInstanceOf[Indexed[(H,I)::T, MaxInd]]
    }
    implicit def ixd [H<:HList,T<:HList,Len<:Nat](implicit  i:Indexed[H,Len],ii:Indexed[T,Len], uu:H<:<HList): Indexed[H::T, Len] = {
      i.asInstanceOf[Indexed[H::T, Len]]
    }
    implicit def hNilInd[N<:Nat]:Indexed[HNil,N] = dfltInd.asInstanceOf[Indexed[HNil,N]]
  }

  implicit class IndexSupport[L<:HList](l:L){
    def index[O<:HList](implicit  hli: RecordIndexer.Aux[L,_0,_,O]):O = hli(l)
  }


  implicit class CSVParserBuilder[Schema <: HList, FldParsers <: HList, Len <: Nat](schema: Schema)
    (implicit mapper: Mapper.Aux[getFldParserByType.type, Schema, FldParsers], i: RecordIndexer.Aux[Schema, _0, Len, _]) {

      class BuilderAux[FP <: HList, ZCO <: HList](zc: ZipConst.Aux[Vector[String], FP, ZCO], fldParsers: FP) {

        def build(tokenize: Tokenizer)(implicit ev: Mapper.Aux[applyFldParser.type, ZCO, Schema]) ={
          new CSVParser[Schema] {

            private def prettyErrMsgs[T](s: String): PartialFunction[Throwable, Try[T]] = {
              case e: NoSuchElementException => scala.util.Failure(new Exception(s"CSV parsing failed, not enough fields in : '$s'"))
              case t: Throwable => scala.util.Failure(new Exception(s"CSV parsing failed for string: '$s'", t))
            }

            def parse(line: String): Try[Schema] = Try {
              fldParsers.zipConst(tokenize(line))(zc).map(applyFldParser)
            }.recoverWith(prettyErrMsgs(line))

          }
        }
      }

      //TODO(problem): remove indexing(RecordIndexer, Indexed), the length of the input row is not correlatong with schema arity!!
      def setupParser[ZCO <: HList, FP <: HList](f: FldParsers => FP)(implicit zc: ZipConst.Aux[Vector[String], FP, ZCO], i: Indexed[FP, Len]) = {
        new BuilderAux[FP, ZCO](zc, f(schema.map(getFldParserByType)))
      }

      def defaultParser[ZCO <: HList,I <: HList, E <: Nat](implicit hli:RecordIndexer.Aux[FldParsers, _0, E, I], zc: ZipConst.Aux[Vector[String], I, ZCO]) = {
        setupParser(hli(_))(zc, dfltInd.asInstanceOf[Indexed[I,Len]])
      }
  }

}