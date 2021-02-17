package tapl

import scala.annotation.tailrec

object Untyped {
  //chapter 5 - 7
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
          case TmAbs(x, t1)  => TmAbs(x, walk(d, c + 1, t1))
          case TmApp(t1, t2) => TmApp(walk(d, c, t1), walk(d, c, t2))
        }
      walk(d, 0, self)
    }
  }

  final case class TmVar(index: Int, contextLength: Int) extends Term
  final case class TmAbs(nameHint: String, t: Term) extends Term
  final case class TmApp(t1: Term, t2: Term) extends Term

  case class Context(contexts: List[String]) {
    def pickFreshName(nameHint: String): (Context, String) = {
      if (contexts.contains(nameHint))
        pickFreshName(nameHint + "'")
      else {
        (Context(contexts :+ nameHint), nameHint)
      }
    }

    def len(): Int = contexts.size

    def index2name(index: Int): String = {
      if (index < len)
        contexts(len() - 1 - index)
      else {
        throw new IndexOutOfBoundsException(
          s"variable lookup fail. index:$index, context length:${len()}"
        )
      }
    }

    def name2index(s: String): Int = {
      contexts.zipWithIndex.find(_._1 == s) match {
        case Some((_, i)) => len() - 1 - i
        case None =>
          throw new IllegalArgumentException(s"variable lookup fails: $s")
      }
    }

  }

  def termString(ctx: Context, term: Term): String =
    term match {
      case TmAbs(x, t) =>
        val (ctx1, x1) = ctx.pickFreshName(x)
        s"(lambda $x1. ${termString(ctx1, t)})"
      case TmVar(x, n) =>
        if (ctx.len() == n)
          s"${ctx.index2name(x)}"
        else {
          throw new IndexOutOfBoundsException("[bad index]")
        }
      case TmApp(t1, t2) =>
        s"(${termString(ctx, t1)} ${termString(ctx, t2)})"
    }
}
