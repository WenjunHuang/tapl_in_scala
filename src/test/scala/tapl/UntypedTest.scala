package tapl

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should
import tapl.core.{Context, Term, TmAbs, TmApp, TmVar, Ty, TyBool, termString}
import tapl.parser._

class UntypedTest extends AnyFlatSpec with should.Matchers {
  import UntypedTest._

  "termString" should "output correct format string" in {
    val data = Seq(
      (lambda("x", TyBool, variable(0, 1)), "(lambda x: Bool. x)"),
      (
        lambda("a", TyBool, apply(TmVar(0, 1), TmVar(0, 1))),
        "(lambda a: Bool. (a a))"
      ),
      (
        lambda(
          "a",
          TyBool,
          lambda("b", TyBool, apply(variable(0, 2), variable(1, 2)))
        ),
        "(lambda a: Bool. (lambda b: Bool. (b a)))"
      ),
      (
        lambda(
          "a",
          TyBool,
          lambda("a", TyBool, apply(variable(0, 2), variable(1, 2)))
        ),
        "(lambda a: Bool. (lambda a': Bool. (a' a)))"
      ),
      (
        apply(
          lambda("a", TyBool, variable(0, 1)),
          lambda("a", TyBool, apply(variable(0, 1), variable(0, 1)))
        ),
        "((lambda a: Bool. a) (lambda a: Bool. (a a)))"
      ),
      (
        apply(
          lambda("a", TyBool, variable(0, 1)),
          lambda("b", TyBool, apply(variable(0, 1), variable(0, 1)))
        ),
        "((lambda a: Bool. a) (lambda b: Bool. (b b)))"
      )
    )

    for ((term, str) <- data) {
      val result = termString(Context(Nil), term)
      result should be(str)
    }
  }

  "shift" should "shift index" in {
    val data = Seq(
      (
        lambda("a", TyBool, variable(0, 1)),
        lambda("a", TyBool, variable(0, 2))
      ),
      // no free variables
      (
        lambda(
          "a",
          TyBool,
          lambda("b", TyBool, apply(variable(0, 2), variable(1, 2)))
        ),
        lambda(
          "a",
          TyBool,
          lambda("b", TyBool, apply(variable(0, 3), variable(1, 3)))
        )
      ),
      // one free variable
      (
        lambda(
          "a",
          TyBool,
          lambda(
            "b",
            TyBool,
            apply(
              variable(0, 2),
              // free variable's index should be shifted
              variable(2, 2)
            )
          )
        ),
        lambda(
          "a",
          TyBool,
          lambda("b", TyBool, apply(variable(0, 3), variable(3, 3)))
        )
      )
    )
    for ((term, expected) <- data)
      term.shift(1) should be(expected)
  }

  "parse" should "work" in {
    startParse("(lambda x: Bool. x)") should be(
      Some(lambda("x", TyBool, variable(0, 1)))
    )
    startParse("((lambda x: Bool. x) (lambda x: Bool.x))") should be(
      Some(
        apply(
          lambda("x", TyBool, variable(0, 1)),
          lambda("x", TyBool, variable(0, 1))
        )
      )
    )
  }
}

object UntypedTest {
  def variable(i: Int, ctxLen: Int) = TmVar(i, ctxLen)
  def lambda(x: String, ty: Ty, t: Term) = TmAbs(x, ty, t)
  def apply(t0: Term, t1: Term) = TmApp(t0, t1)
}
