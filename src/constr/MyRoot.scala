package constr

import shapeless._

sealed trait MyRoot { val e: String }

case class ChildA(e: String) extends MyRoot
case class ChildB(e: String) extends MyRoot
case class ChildC(e: String) extends MyRoot

