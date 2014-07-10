package constr

import shapeless._
import shapeless.ops.hlist.Selector


sealed trait XbrliObject { val e: String }

case class Fact(e: String) extends XbrliObject
case class Context(e: String) extends XbrliObject
case class Item(e: String) extends XbrliObject


sealed abstract class Foo(e: String)
case class Bar extends Foo("1")
case class Bazz extends Foo("2")



trait SameTypes[C, H <: HList, F[I,_], I] {}

object SameTypes {
  
  class Apply[P, F[I,_]] {
    def apply[H <: HList, C <: Coproduct, I](h: H)(implicit st: SameTypes[C, H, F, I], gen: Generic.Aux[P, C]): SameTypes[C,H,F, I] = st
  }
  def apply[P, F[_,_]] = new Apply[P, F]
  
  implicit def sameNil[F[I,_], I]: SameTypes[CNil, HNil, F, I] = new SameTypes[CNil, HNil, F, I] {}
  
  implicit def sameOther[I, H0, CT <: Coproduct, HT <: HList, F[I,_]](implicit ct: SameTypes[CT, HT, F, I]) = new SameTypes[H0 :+: CT, F[I, H0] :: HT, F, I] {}
}

trait SealedFamilyFactory {
  type SealedFamily 
  implicit val generic: Generic[SealedFamily]
  type Constructors <: HList
  type Input
  val constructors: Constructors
  
  implicit val st: SameTypes[generic.Repr, Constructors, Function1, Input]
  
  def apply[T](input: Input)(implicit sel: Selector[Constructors, Input => T]) = {
    constructors.select[Input => T].apply(input)
  } 
  
}

object XbrliObjectFactory extends SealedFamilyFactory {
  
  type SealedFamily = XbrliObject 
  
  implicit val generic = Generic[XbrliObject]
  type Input = String
  
  val c0: String => Context = { case s: String if s == "contect" => Context(s)}
  val c1: String => Fact = { case s: String if s == "fact" => Fact(s)}
  val c2: String => Item = { case s: String if s == "item" => Item(s)}
  
  type Constructors = (String => Context) :: (String => Fact) :: (String => Item) :: HNil 
  
  val constructors = c0 :: c1 :: c2 :: HNil
  
  val st = SameTypes[XbrliObject, Function1](constructors)
  
  
}

object main extends App {
  val myselector = "fact"
  
  val myFact = XbrliObjectFactory.apply[Fact](myselector)
}

