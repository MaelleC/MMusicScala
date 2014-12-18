package harm

import org.scalatest.FunSuite
import cafesat.api.API._
import gen.Constraints

class ConstraintsTest extends FunSuite {
  test("exactlyOne") {
    val cl = List.range(0, 4) map { _ => boolVar() }
    val d = Constraints.exactlyOne(cl)
    val expStr = "" +
      "List(or(P_0, P_1, P_2, P_3), " +
      "or(not(P_0), not(P_1)), or(not(P_0), not(P_2)), or(not(P_0), not(P_3)), " +
      "or(not(P_1), not(P_2)), or(not(P_1), not(P_3)), or(not(P_2), not(P_3)))"

    assert(d.toString == expStr)

    val res = solveForSatisfiability(and(d: _*))
    assert(res != None)

    val d2 = or(cl(0)) :: or(cl(1)) :: d
    assert(solveForSatisfiability(and(d2: _*)) == None)

    // test when only one already
    val e = Constraints.exactlyOne(List(boolVar()))
    assert(solveForSatisfiability(and(e: _*)) != None)

  }

  test("exactlyOnePair") {
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
    //to run with "exactlyOne" test (for the incdices of bool vars)
    //assert(d.toString == expStr)

    //prove c1 && c2 <=> p by contradiction
    val dCToP = or(cl(0)) :: or(cl(1)) :: or(!cpp(0)) :: d //(c1 and c2) => !p
    val dPToC = or(!cl(0)) :: or(!cl(1)) :: or(cpp(0)) :: d //p => !(c1 and c2) <=> p => !c1 || !c2

    assert(solveForSatisfiability(and(dCToP: _*)) == None)
    assert(solveForSatisfiability(and(dPToC: _*)) == None)

    //when only one pair possible
    val c1 = List.range(0, 2) map { _ => boolVar() }
    val cp1 = List((c1(0), c1(1)))
    val e = Constraints.exactlyOnePair(cp1, List(boolVar()))
    assert(solveForSatisfiability(and(e: _*)) != None)

  }
}