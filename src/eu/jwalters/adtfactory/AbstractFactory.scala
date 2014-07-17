package eu.jwalters.adtfactory

import shapeless.HList
import shapeless.ops.hlist.Selector
import shapeless.ops.hlist.ToList

class AbstractFactory[Input] {
  
  /**
   * Deferred class providing the actual implementation, using type parameters that can be inferred by the compiler. 
   */
  protected class FactoryOps[Constructors <: HList, UB](constructors: Constructors) {

    class CreateOfType[T] {
      def apply[Repr](input: Input)(implicit sel: Selector[Constructors, Input => Option[T]]) = {
        constructors.select[Input => Option[T]].apply(input)
      }
    }

    def createOfType[T] = new CreateOfType[T]

    def createAny[Repr](input: Input)(implicit lub: ToList[Constructors, (Input => Option[UB])]) = {
      constructors.toList(lub).flatMap(a => a(input)).headOption
    }
  }
  
}