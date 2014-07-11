package constr

import shapeless._
import shapeless.ops.hlist.{ Selector, ToList }

trait CoproductConstructable[C, H <: HList] {}

object CoproductConstructable {
  implicit def sameNil: CoproductConstructable[CNil, HNil] = new CoproductConstructable[CNil, HNil] {}
  implicit def sameOther[H0, CT <: Coproduct, HT <: HList](implicit ct: CoproductConstructable[CT, HT]) = new CoproductConstructable[H0 :+: CT, (String => Option[H0]) :: HT] {}
}

class ADTFactory[Input, ADTRoot] {

  def apply[Constructors <: HList, Repr](c: Constructors)(implicit gen: Generic.Aux[ADTRoot, Repr], con: CoproductConstructable[Repr, Constructors]) = new Factory[Constructors, Repr](c)

  class Factory[Constructors <: HList, Repr](constructors: Constructors)(implicit gen: Generic.Aux[ADTRoot, Repr], con: CoproductConstructable[Repr, Constructors]) {

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
  def apply[I, A] = new ADTFactory[I, A]()
}





