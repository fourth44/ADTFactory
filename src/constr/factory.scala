package constr

import shapeless._
import shapeless.ops.hlist._

sealed trait MyRoot { val e: String }

case class ChildOne(e: String) extends MyRoot
case class ChildTwo(e: String) extends MyRoot
case class Item(e: String) extends MyRoot

object MyRoot {
  implicit val generic = Generic[MyRoot]
}

object MyRootFactory extends SealedFamilyFactory[String, MyRoot] {

  def opt[A](pf: PartialFunction[String, A]) = pf.lift
  
  type Constructors = (String => Option[ChildOne]) :: (String => Option[ChildTwo]) :: (String => Option[Item]) :: HNil
  val constructors =
    opt { case s: String if s == "one" => ChildOne(s) } ::
      opt { case s: String if s == "two" => ChildTwo(s) } ::
      opt { case s: String if s == "three" => Item(s) } ::
      HNil

}

object main extends App {
  val myselector = "two"

  val myChildTwo: Option[ChildTwo] = MyRootFactory[ChildTwo](myselector)
}
