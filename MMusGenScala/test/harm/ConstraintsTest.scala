package harm

import org.scalatest.FunSuite
import cafesat.api.API._
import gen.Constraints

class ConstraintsTest extends FunSuite {
  test("exactlyOne") {
    val d = Constraints.exactlyOne(List.range(0, 4) map { _ => boolVar() })
    val expStr = "" +
      "List(or(P_0, P_1, P_2, P_3), " +
      "or(not(P_0), not(P_1)), or(not(P_0), not(P_2)), or(not(P_0), not(P_3)), " +
      "or(not(P_1), not(P_2)), or(not(P_1), not(P_3)), or(not(P_2), not(P_3)))"

    assert(d.toString == expStr)
  }

  test("exactlyOnePair") { //to run with "exactlyOne" test (for the incdices of bool vars)
    val cl = List.range(0, 4) map { _ => boolVar() }
    val cp = (cl(0), cl(1)) :: (cl(1), cl(3)) :: (cl(2), cl(0)) :: Nil
    val cpp = cp map { _ => boolVar() }

    val d = Constraints.exactlyOnePair(cp, cpp)

    val expStr = "" +
      "List(or(not(P_8), P_4), or(not(P_8), P_5), or(P_8, not(P_4), not(P_5)), " +
      "or(not(P_9), P_5), or(not(P_9), P_7), or(P_9, not(P_5), not(P_7)), " +
      "or(not(P_10), P_6), or(not(P_10), P_4), or(P_10, not(P_6), not(P_4)), " +
      "or(P_8, P_9, P_10), " +
      "or(not(P_8), not(P_9)), or(not(P_8), not(P_10)), or(not(P_9), not(P_10)))"
    assert(d.toString == expStr)
  }
}