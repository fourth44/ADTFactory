package constr

import shapeless._
import shapeless.ops.hlist.Selector


sealed trait XbrliObject { val e: String }

case class Fact(e: String) extends XbrliObject
case class Context(e: String) extends XbrliObject
case class Item(e: String) extends XbrliObject

trait SameTypes[C, H <: HList, F[_,_], W[_]] {}

object SameTypes {
  
  class Apply[P, F[_,_], W[_]] {
    def apply[H <: HList, C <: Coproduct, I](h: H)(implicit st: SameTypes[C, H, F, W], gen: Generic.Aux[P, C]): SameTypes[C,H,F, W] = st
  }
  def apply[P, F[_,_], W[_]] = new Apply[P, F, W]
  
  implicit def sameNil[F[_,_], W[_]]: SameTypes[CNil, HNil, F, W] = new SameTypes[CNil, HNil, F, W] {}
  
  implicit def sameOther[I, H0, CT <: Coproduct, HT <: HList, F[_,_], W[_]](implicit ct: SameTypes[CT, HT, F, W]) = new SameTypes[H0 :+: CT, F[I, W[H0]] :: HT, F, W] {}
}

trait SealedFamilyFactory {
  type SealedFamily 
  implicit val generic: Generic[SealedFamily]
  type Constructors <: HList
  type Input
  val constructors: Constructors
  type F[_,_]
  type O[_]
  
  implicit val st: SameTypes[generic.Repr, Constructors, F, O]
  
  def apply[T](input: Input)(implicit sel: Selector[Constructors, Input => Option[T]]) = {
    constructors.select[Input => Option[T]].apply(input)
  } 
  
}

object XbrliObjectFactory extends SealedFamilyFactory {
  
  type SealedFamily = XbrliObject 
  
  implicit val generic = Generic[XbrliObject]
  type Input = String
  
  type F[A,B] = Function1[A,B]
  type O[A] = Option[A]
  
  def opt[A](pf: PartialFunction[Input, A]) = pf.lift
  
  val c0 = opt { case s: String if s == "contect" => Context(s)}
  val c1 = opt { case s: String if s == "fact" => Fact(s)}
  val c2 = opt { case s: String if s == "item" => Item(s)}
  
  type Constructors = (String => Option[Context]) :: (String => Option[Fact]) :: (String => Option[Item]) :: HNil 
  val constructors = c0 :: c1 :: c2 :: HNil
  
  val st = SameTypes[XbrliObject, F, Option](constructors)
  
}

object main extends App {
  val myselector = "fact"
  
  val myFact: Option[Fact] = XbrliObjectFactory[Fact](myselector)
}

