package tapl

import scala.util.Try

object Arith {
  sealed trait Term
  case object True extends Term
  case object False extends Term
  case class If(term1: Term, term2: Term, term3: Term) extends Term
  case object Zero extends Term
  case class Succ(term: Term) extends Term
  case class Pred(term: Term) extends Term
  case class IsZero(term: Term) extends Term

  def isNumericVal(term: Term): Boolean =
    term match {
      case Zero     => true
      case Succ(t1) => isNumericVal(t1)
      case _        => false
    }

  def isVal(term: Term): Boolean =
    term match {
      case True                 => true
      case False                => true
      case t if isNumericVal(t) => true
      case _                    => false
    }

  def eval1(t: Term): Term =
    t match {
      case If(True, t2, t3)  => t2
      case If(False, t2, t3) => t3
      case If(t1, t2, t3) =>
        If(eval1(t1), t2, t3)
      case Succ(t1) =>
        Succ(eval1(t1))
      case Pred(Zero) =>
        Zero
      case Pred(Succ(nv1)) if isNumericVal(nv1)   => nv1
      case Pred(t1)                               => Pred(eval1(t1))
      case IsZero(Zero)                           => True
      case IsZero(Succ(nv1)) if isNumericVal(nv1) => False
      case IsZero(t1) =>
        IsZero(eval1(t1))
    }

  def eval(t: Term): Term =
    Try {
      eval1(t)
    }.recover {
      case _ => t
    }.get
}
