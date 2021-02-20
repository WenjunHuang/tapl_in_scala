package tapl

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should
import tapl.core.{TmAbs, TmVar, TyArrow, TyBool}
import tapl.parser.startParse

class TypeofTest extends AnyFlatSpec with should.Matchers {
  "typeof " should "work" in {
    startParse("(lambda x: (Bool -> Bool).x)") should be(Some(TmAbs("x",TyArrow(TyBool,TyBool),TmVar(0,1))))
  }

}
