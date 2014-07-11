package constr

import shapeless._
import shapeless.ops.hlist._

sealed trait MyRoot { val e: String }

case class ChildA(e: String) extends MyRoot
case class ChildB(e: String) extends MyRoot
case class ChildC(e: String) extends MyRoot

object MyRoot {
  implicit val generic = Generic[MyRoot]
}

object MyRootFactory extends SealedFamilyFactory[String, MyRoot] {

  def opt[A](pf: PartialFunction[String, A]) = pf.lift
  
  type Constructors = (String => Option[ChildA]) :: (String => Option[ChildB]) :: (String => Option[ChildC]) :: HNil
  val constructors =
    opt { case s: String if s == "one" => ChildA(s) } ::
      opt { case s: String if s == "two" => ChildB(s) } ::
      opt { case s: String if s == "three" => ChildC(s) } ::
      HNil

}

object main extends App {
  val myChildB: Option[ChildB] = MyRootFactory.createOfType[ChildB]("two")
  val myNone:     Option[ChildB] = MyRootFactory.createOfType[ChildB]("foo")
  
  val myUnknown = MyRootFactory.createAny("three")
}
