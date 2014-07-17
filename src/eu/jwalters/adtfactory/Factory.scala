package eu.jwalters.adtfactory

import shapeless._
import shapeless.ops.hlist.ToList

class Factory[Input] extends AbstractFactory[Input] {
  def apply[H <: HList, LUB, O <: HList](c: H)(implicit constr: ConstructableTC.HListConstructable.Constructable.Aux[Input, H, O], list: ToList[O, LUB]) = new FactoryOps[H, LUB](c)
}

object Factory {
  def apply[T] = new Factory[T]
}
