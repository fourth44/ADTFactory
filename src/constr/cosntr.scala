package constr

import shapeless._
import shapeless.ops.hlist.{Selector, ToList}


trait CoproductConstructable[C, H <: HList] {}

object CoproductConstructable {
  implicit def sameNil: CoproductConstructable[CNil, HNil] = new CoproductConstructable[CNil, HNil] {}
  implicit def sameOther[H0, CT <: Coproduct, HT <: HList](implicit ct: CoproductConstructable[CT, HT]) = new CoproductConstructable[H0 :+: CT, (String => Option[H0]) :: HT] {}
}

trait SealedFamilyFactory[Input, SealedFamily] {
  
  type Constructors <: HList
  val constructors: Constructors
  
  class Create[T] {
    def apply[Repr](input: Input)(implicit sel: Selector[Constructors, Input => Option[T]], 
        gen: Generic.Aux[SealedFamily, Repr],
        con: CoproductConstructable[Repr, Constructors]) = {
      constructors.select[Input => Option[T]].apply(input)
    }
  }
  
  def createOfType[T] = new Create[T]
  
  def createAny[Repr](input: Input)(implicit gen: Generic.Aux[SealedFamily, Repr],
        con: CoproductConstructable[Repr, Constructors],
        lub: ToList[Constructors,(Input => Option[SealedFamily])]) = {
    constructors.toList(lub).flatMap(a => a(input)).headOption
  } 
}



