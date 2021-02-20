package tapl.core
/* types */

sealed trait Ty
case object TyBool extends Ty
final case class TyArrow(left: Ty, right: Ty) extends Ty // t1 -> t2

