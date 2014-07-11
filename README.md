ADTFactory
==========

Proof of Concept for assisting in creating type-safe factories for a single-level ADT using Shapeless.
The ADT should be a sealed trait or sealed abstract class with case class members. 

A concrete factory is obtained by creating a ADTFactory providing a HList of constructor types for each ADT member (InputType => Option[MemberType]). Note that the HList should be ordered alphabetically according to the type names of the members, and cannot provide any constuctor functions to types outside the ATD. Both limitations can be lifted but was not of interest for this PoC.

Example:
```scala

import shapeless._

sealed trait MyRoot { val e: String }

case class ChildA(e: String) extends MyRoot
case class ChildB(e: String) extends MyRoot
case class ChildC(e: String) extends MyRoot

implicit val generic = Generic[MyRoot]

def opt[A](pf: PartialFunction[String, A]) = pf.lift

val constructors =
  opt { case s: String if s == "one" => ChildA(s) } ::
  opt { case s: String if s == "two" => ChildB(s) } ::
  opt { case s: String if s == "three" => ChildC(s) } ::
  HNil
  
val myRootFactory = ADTFactory[String, MyRoot](constructors)

val myChildB: Option[ChildB] = myRootFactory.createOfType[ChildB]("two")
val myNone: Option[ChildB] = myRootFactory.createOfType[ChildB]("one")
val myUnknown = myRootFactory.createAny("three")
val myNotThere = myRootFactory.createAny("four")

assert(myChildB == Some(ChildB("two")))
assert(myNone == None)
assert(myUnknown == Some(ChildC("three")))
assert(myNotThere == None)

```

