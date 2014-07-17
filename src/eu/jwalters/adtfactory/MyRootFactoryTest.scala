package eu.jwalters.adtfactory

import shapeless._

object MyFactoryTest extends App {

  def opt[A](pf: PartialFunction[String, A]) = pf.lift

  implicit val generic = Generic[MyRoot]

  /**
   * Create some constructors. Be sure to provide a HList of type ('inputtype' => Option['membertype']),
   * alphabetically sorted on membertype.
   */

  val constructors =
      opt { case s: String if s == "one" => ChildA(s) } ::
      opt { case s: String if s == "two" => ChildB(s) } ::
      opt { case s: String if s == "three" => ChildC(s) } ::
      HNil

  /**
   * Using the constructors we can make an actual factory. Doesn't work if they do not match the subtypes of the root type.
   * A Generic['roottype'] must be in scope.
   */

  val myRootFactory = ADTFactory[String, MyRoot](constructors)

  /**
   * Now let's use the factory
   */

  val myChildB: Option[ChildB] = myRootFactory.createOfType[ChildB]("two")
  val myNone: Option[ChildB] = myRootFactory.createOfType[ChildB]("one")
  val myUnknown = myRootFactory.createAny("three")
  val myNotThere = myRootFactory.createAny("four")

  assert(myChildB == Some(ChildB("two")))
  assert(myNone == None)
  assert(myUnknown == Some(ChildC("three")))
  assert(myNotThere == None)

  println(myChildB)
  println(myNone)
  println(myUnknown)
  println(myNotThere)
  
  /**
   * And now the non-ADT factory: basically the same except not all member types need to be present 
   * (or in alphabetic order for that manner), and the least upper bound type is inferred.
   */
  
  val myRootFactory2 = Factory[String](constructors)

}
