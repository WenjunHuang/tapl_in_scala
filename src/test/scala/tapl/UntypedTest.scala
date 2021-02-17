package tapl

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should
import Untyped._

class UntypedTest extends AnyFlatSpec with should.Matchers {
  import UntypedTest._

  "termString" should "output correct format string" in {
    val data = Seq(
      (abstraction("x", variable(0, 1)), "(lambda x. x)"),
      (abstraction("a", apply(TmVar(0, 1), TmVar(0, 1))), "(lambda a. (a a))"),
      (
        abstraction(
          "a",
          abstraction("b", apply(variable(0, 2), variable(1, 2)))
        ),
        "(lambda a. (lambda b. (b a)))"
      ),
      (
        abstraction(
          "a",
          abstraction("a", apply(variable(0, 2), variable(1, 2)))
        ),
        "(lambda a. (lambda a'. (a' a)))"
      ),
      (
        apply(
          abstraction("a", variable(0, 1)),
          abstraction("a", apply(variable(0, 1), variable(0, 1)))
        ),
        "((lambda a. a) (lambda a. (a a)))"
      ),
      (
        apply(
          abstraction("a", variable(0, 1)),
          abstraction("b", apply(variable(0, 1), variable(0, 1)))
        ),
        "((lambda a. a) (lambda b. (b b)))"
      )
    )

    for ((term, str) <- data) {
      val result = termString(Context(Nil), term)
      result should be(str)
    }
  }

  "shift" should "shift index" in {
    val data = Seq(
      (abstraction("a", variable(0, 1)), abstraction("a", variable(0, 2))),
      // no free variables
      (
        abstraction(
          "a",
          abstraction("b", apply(variable(0, 2), variable(1, 2)))
        ),
        abstraction(
          "a",
          abstraction("b", apply(variable(0, 3), variable(1, 3)))
        )
      ),
      // one free variable
      (
        abstraction(
          "a",
          abstraction("b", apply(variable(0, 2),
            // free variable's index should be shifted
            variable(2, 2)))
        ),
        abstraction(
          "a",
          abstraction("b", apply(variable(0, 3), variable(3, 3)))
        )
      )
    )
    for ((term, expected) <- data)
      term.shift(1) should be(expected)
  }
}

object UntypedTest {
  def variable(i: Int, ctxLen: Int) = TmVar(i, ctxLen)
  def abstraction(x: String, t: Term) = TmAbs(x, t)
  def apply(t0: Term, t1: Term) = TmApp(t0, t1)
}
