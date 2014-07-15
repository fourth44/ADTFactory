package eu.jwalters.adtfactory

import shapeless._
import shapeless.ops.hlist.Comapped
import shapeless.ops.hlist.ToList

trait Constr[Input, HList] // { type Out }

object Constr {
  type Aux[A, B] = Constr[A, B] // { type Out = B }
  def apply[I, H <: HList](implicit constr: Constr.Aux[I, H]) = constr
  implicit def hnil[A] = new Constr[A, HNil] {} // { type Out = HNil}
  implicit def hlist[I, H, HT <: HList](implicit constr: Constr.Aux[I, HT]) = new Constr[I, (I => Option[H]) :: HT] {} //{ type Out = H :: OT }
}

class Factory[Input] {
  
  def apply[H <: HList, LUB](c: H)(implicit constr: Constr.Aux[Input, H], list: ToList[H, LUB]) = new Inner(c)
  
  class Inner[H <: HList, LUB](c: H)(implicit constr: Constr.Aux[Input, H], list: ToList[H, LUB]) {
    val aslist = list.apply(c)
  }
}

object Factory {
  def apply[T] = new Factory[T]
}

object FactoryTest {

  def opt[A, B](pf: PartialFunction[A, B]) = pf.lift

  val constructors =
    opt[String, ChildA]{ case s: String if s == 3.2 => ChildA(s) } ::
      opt[String, ChildB] { case s: String if s == "two" => ChildB(s) } ::
      opt[String, ChildC] { case s: String if s == "three" => ChildC(s) } ::
      HNil

  val factory = Factory[String](constructors)

  val list = factory.aslist

}