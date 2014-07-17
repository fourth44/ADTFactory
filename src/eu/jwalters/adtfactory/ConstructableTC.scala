package eu.jwalters.adtfactory

import shapeless._

trait ConstructableTC[OutList] {
  type Cons[_, _ <: OutList] <: OutList
  type Nil <: OutList

  /**
   * Witnesses that every element X in Coproduct C is matched by an element in HList H of the form (I => Option[X])
   */
  trait Constructable[Input, H <: HList] { type Out <: OutList }

  object Constructable {
    type Aux[Input, H <: HList, C <: OutList] = Constructable[Input, H] { type Out = C }

    implicit def hnil[Input]: Constructable.Aux[Input, HNil, Nil] = new Constructable[Input, HNil] { type Out = Nil }

    implicit def hlist[Input, H, HT <: HList, OT <: OutList](implicit constr: Constructable.Aux[Input, HT, OT]) =
      new Constructable[Input, (Input => Option[H]) :: HT] { type Out = Cons[H, OT] }
  }
}

object ConstructableTC {
  
  val HListConstructable = new ConstructableTC[HList] {
    type Cons[A, B <: HList] = ::[A, B]
    type Nil = HNil
  }
  val CoproductConstructable = new ConstructableTC[Coproduct] {
    type Cons[A, B <: Coproduct] = :+:[A, B]
    type Nil = CNil
  }

}
