package eu.jwalters.adtfactory

import shapeless._
import shapeless.ops.hlist.{ Selector, ToList }

/**
 * Provides factory methods (indirectly) around a HList of constructors of type (Input => Option[_]),
 * requiring proof that the list of constructors provide for all possible direct subtypes of ADTRoot.
 * Preferred syntax:
 * val factory = ADTFactory[Input, ADTRoot](constructors: HList)
 * val value: Option[M] = factory.createOfType[M](i: Input) // for some member type M of ADTRoot
 */
class ADTFactory[Input, ADTRoot] extends AbstractFactory[Input] {

  def apply[Constructors <: HList, Repr <: Coproduct](c: Constructors)(
    implicit gen: Generic.Aux[ADTRoot, Repr],
    con: ConstructableTC.CoproductConstructable.Constructable.Aux[Input, Constructors, Repr]) =
    new FactoryOps[Constructors, ADTRoot](c)

}

object ADTFactory {

  def apply[Input, ADTRoot] = new ADTFactory[Input, ADTRoot]()
}





