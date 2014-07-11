package eu.jwalters.adtfactory

import shapeless._
import shapeless.ops.hlist.{ Selector, ToList }

/**
 * Witnesses that every element X in Coproduct C is matched by an element in HList H of the form (I => Option[X])
 */
trait CoproductConstructable[I, C, H <: HList] {}

object CoproductConstructable {
  implicit def sameNil[I]: CoproductConstructable[I, CNil, HNil] = new CoproductConstructable[I, CNil, HNil] {}
  implicit def sameOther[I, H0, CT <: Coproduct, HT <: HList](implicit ct: CoproductConstructable[I, CT, HT]) = new CoproductConstructable[I, H0 :+: CT, (I => Option[H0]) :: HT] {}
}

/**
 * Provides factory methods (indirectly) around a HList of constructors of type (Input => Option[_]), 
 * requiring proof that the list of constructors provide for all possible direct subtypes of ADTRoot.
 */
class ADTFactory[Input, ADTRoot] {

  def apply[Constructors <: HList, Repr](c: Constructors)(implicit gen: Generic.Aux[ADTRoot, Repr], con: CoproductConstructable[Input, Repr, Constructors]) = new FactoryOps[Constructors, Repr](c)

  /**
   * Deferred class providing the actual implementation, but using a list of generics that can be inferred by the compiler using Input and ADTRoot. 
   */
  class FactoryOps[Constructors <: HList, Repr](constructors: Constructors)(implicit gen: Generic.Aux[ADTRoot, Repr], con: CoproductConstructable[Input, Repr, Constructors]) {

    class CreateOfType[T] {
      def apply[Repr](input: Input)(implicit sel: Selector[Constructors, Input => Option[T]]) = {
        constructors.select[Input => Option[T]].apply(input)
      }
    }

    def createOfType[T] = new CreateOfType[T]

    def createAny[Repr](input: Input)(implicit lub: ToList[Constructors, (Input => Option[ADTRoot])]) = {
      constructors.toList(lub).flatMap(a => a(input)).headOption
    }
  }
}

object ADTFactory {
  
  def apply[Input, ADTRoot] = new ADTFactory[Input, ADTRoot]()
}





