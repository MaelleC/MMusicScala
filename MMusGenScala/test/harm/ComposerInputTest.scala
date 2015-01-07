package harm

import org.scalatest.FunSuite
import segmentSystem._
import gen._
import chord._
import tonalSystem.Tone._
import gen.Chord._

class ComposerInputTest extends FunSuite {

  test("testCorrectInput") {
    val d = HarmonyGen(EmptySeq)
    val melLen = 12

    //implicit conversion
    val comp: List[(Int, List[CConstr])] = List((1, List(Triad(I))),
      (3, List(Triad(II))),
      (8, List(Triad(III))),
      (9, List(Triad(IV))))

    val compafter = d.getConsList(comp, melLen)

    assert(compafter.length == melLen)
    comp map { x => assert(compafter(x._1) == x._2) }
    (compafter filter (x => !((comp map { y => y._1 }) contains (compafter.indexOf(x))))) map { x => assert(x == List(NoCons)) }

  }

  test("testBadInput") {
    val d = HarmonyGen(EmptySeq)
    val melLen = 12
    val compmin = List((1, List(ChInvPoss(Triad(I), Set(Fond)))),
      (3, List(ChInvPoss(Triad(II), Set(Fond)))),
      (-1, List(ChInvPoss(Triad(III), Set(Fond)))),
      (9, List(ChInvPoss(Triad(IV), Set(Fond)))))

    //d.getConsList(compmin, melLen) //intercept does not work for error("")

    val compmax = List((1, List(ChInvPoss(Triad(I), Set(Fond)))),
      (3, List(ChInvPoss(Triad(II), Set(Fond)))),
      (melLen, List(ChInvPoss(Triad(III), Set(Fond)))),
      (9, List(ChInvPoss(Triad(IV), Set(Fond)))))

    //d.getConsList(compmax, melLen) //same

    val compdup = List((1, List(ChInvPoss(Triad(I), Set(Fond)))),
      (3, List(ChInvPoss(Triad(II), Set(Fond)))),
      (8, List(ChInvPoss(Triad(III), Set(Fond)))),
      (8, List(ChInvPoss(Triad(III), Set(Fond)))),
      (9, List(ChInvPoss(Triad(IV), Set(Fond)))))

    val compafter = d.getConsList(compdup, melLen)
    assert(compafter.length == melLen)
    println(compafter)

    val comporder = List((1, List(ChInvPoss(Triad(I), Set(Fond)))),
      (3, List(ChInvPoss(Triad(II), Set(Fond)))),
      (8, List(ChInvPoss(Triad(III), Set(Fond)))),
      (5, List(ChInvPoss(Triad(III), Set(Fond)))),
      (9, List(ChInvPoss(Triad(IV), Set(Fond)))))

    val compaftero = d.getConsList(comporder, melLen)
    assert(compaftero.length == melLen)
    println(compaftero)

    val compEmpty = List((1, List(ChInvPoss(Triad(I), Set(Fond)))),
      (3, List(ChInvPoss(Triad(II), Set(Fond)), NoCons)),
      (8, List(ChInvPoss(Triad(III), Set(Fond)))),
      (9, List(ChInvPoss(Triad(IV), Set(Fond)))))

    val compaftere = d.getConsList(compEmpty, melLen)
    assert(compaftere.length == melLen)
    println(compaftere)

  }

}