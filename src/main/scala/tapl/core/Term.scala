package tapl.core

import tapl.core.Term.termSubstTop

sealed trait EvalStop
case object NoRuleApplies extends EvalStop

sealed trait Term { self =>
  def shift(d: Int): Term = {
    def walk(d: Int, c: Int, t: Term): Term =
      t match {
        case TmVar(x, n) =>
          if (x >= c) {
            TmVar(x + d, n + d)
          } else {
            TmVar(x, n + d)
          }
        case TmAbs(x, ty, t1) => TmAbs(x, ty, walk(d, c + 1, t1))
        case TmApp(t1, t2)    => TmApp(walk(d, c, t1), walk(d, c, t2))
      }
    walk(d, 0, self)
  }

  def substitute(j: Int, term: Term): Term = {
    def walk(j: Int, s: Term, c: Int, t: Term): Term = {
      t match {
        case TmVar(x, n) =>
          if (x == j + c)
            s.shift(c)
          else TmVar(x, n)
        case TmAbs(x, ty, t1) => TmAbs(x, ty, walk(j, s, c + 1, t1))
        case TmApp(t1, t2)    => TmApp(walk(j, s, c, t1), walk(j, s, c, t2))
      }

    }
    walk(j, term, 0, self)
  }

  def isValue(ctx: Context): Boolean =
    self match {
      case TmAbs(_, _, _) => true
      case _              => false
    }

  def typeof(ctx: Context): Either[String, Ty] =
    self match {
      case TmTrue  => Right(TyBool)
      case TmFalse => Right(TyBool)
      case TmIf(condition, term1, term2) =>
        for {
          ty <- condition.typeof(ctx)
          _ <-
            if (ty == TyBool) Right(ty)
            else Left("guard of conditional not a boolean")
          tyT1 <- term1.typeof(ctx)
          tyT2 <- term2.typeof(ctx)
          result <-
            if (tyT1 == tyT2) Right(tyT2)
            else
              Left("arms of conditional have different types")
        } yield result
      case TmVar(i, _) => ctx.getTypeFromContext(i)
      case TmAbs(x, tyT1, t) =>
        val newCtx = ctx.addBinding(x, VarBind(tyT1))
        t.typeof(newCtx).map(tyT2 => TyArrow(tyT1, tyT2))
      case TmApp(t1, t2) =>
        (for {
          tyT1 <- t1.typeof(ctx)
          tyT2 <- t2.typeof(ctx)
        } yield {
          tyT1 match {
            case TyArrow(tyT11, tyT12) =>
              if (tyT2 == tyT11) Right(tyT12)
              else Left("parameter type mismatch")
            case _ => Left("array type expected")
          }
        }).joinRight
      case TmLet(_, t1, t2) =>
        for {
          _ <- t1.typeof(ctx)
          tyT2 <- t2.typeof(ctx)
        } yield tyT2
    }

  def eval(ctx: Context): Either[EvalStop, Term] =
    self match {
      case TmApp(TmAbs(_, _, t12), v2) if v2.isValue(ctx) =>
        Right(termSubstTop(v2, t12))
      case TmApp(v1, t2) if v1.isValue(ctx) =>
        t2.eval(ctx).flatMap { it => Right(TmApp(v1, it)) }
      case TmApp(t1, t2) => t1.eval(ctx).flatMap { it => Right(TmApp(it, t2)) }
      case _             => Left(NoRuleApplies)
    }
}
object Term {
  def termSubstTop(s: Term, t: Term): Term =
    t.substitute(0, s.shift(1)).shift(-1)
}

case object TmTrue extends Term
case object TmFalse extends Term
final case class TmIf(condition: Term, term1: Term, term2: Term) extends Term
final case class TmVar(index: Int, contextLength: Int) extends Term
final case class TmAbs(x: String, ty: Ty, t: Term) extends Term
final case class TmApp(t1: Term, t2: Term) extends Term
final case class TmLet(x: String, term1: Term, term3: Term) extends Term
