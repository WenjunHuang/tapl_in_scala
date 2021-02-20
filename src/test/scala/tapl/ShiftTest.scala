package tapl

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should
import tapl.core.TmVar

class ShiftTest extends AnyFlatSpec with should.Matchers {
  "shift function" should "work" in {
    val variable = TmVar(0,1)
    variable.shift(1) should be(TmVar(1,2))

  }

}
