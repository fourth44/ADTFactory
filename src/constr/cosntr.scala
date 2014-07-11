package constr

import shapeless._
import shapeless.ops.hlist.Selector


trait CoproductConstructable[C, H <: HList] {}

object CoproductConstructable {
  implicit def sameNil: CoproductConstructable[CNil, HNil] = new CoproductConstructable[CNil, HNil] {}
  implicit def sameOther[H0, CT <: Coproduct, HT <: HList](implicit ct: CoproductConstructable[CT, HT]) = new CoproductConstructable[H0 :+: CT, (String => Option[H0]) :: HT] {}
}

trait SealedFamilyFactory[Input, SealedFamily] {
  
  type Constructors <: HList
  val constructors: Constructors
  
  class Apply[T] {
    def apply[Repr](input: Input)(implicit sel: Selector[Constructors, Input => Option[T]], 
        gen: Generic.Aux[SealedFamily, Repr],
        con: CoproductConstructable[Repr, Constructors]) = {
      constructors.select[Input => Option[T]].apply(input)
    }
  }
  
  def apply[T] = new Apply[T]
  
}



