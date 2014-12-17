package harm
import org.scalatest.FunSuite
import tonalSystem.Tone._
import segmentSystem._
import utils.MelodyWriter
import rythmics._
import gen._
import midiInterface.MelodyPlayer
import chord._

class Harm extends FunSuite with MelodyWriter {
  implicit val noteDuration = E

  val part1 = {
    (O + III + IV
      + V / (2 / 3.0) + IV / (2.0) + III
      + III + I + (VII - 7)
      + (VII - 7) / 2.0 + I / (2 / 3.0) + III
      + IV(Q) + III(S) + IV(S)
      + V / (2 / 3.0) + VI / (2.0) + VII
      + V + III + II
      + I / (1 / 4.0))
  }

  val part1harm = {
    (O
      + V
      + III
      + I
      + IV
      + V
      + V
      + I / (1 / 2.0)) / (1 / 3.0)
  }

  val d = HarmonyGen(part1harm)
  val mel = d.getMel(part1harm)

  test("get flat melody") {
    assert(mel.parDepth == 0)
  }

  val melT = mel.notes map (_.tone)
  val possibleChords: List[List[ChInv]] = melT map (d.getPossChords(_))

  //println(possibleChords)
  //seems ok
  /*
  List(
    List(ChInv(EmptyChord, List())),
    List(ChInv(Triad(I(0, None)), Set(Fond, Inv1, Inv2)),
      ChInv(Triad(III(0, None)), Set(Fond, Inv1)),
      ChInv(Triad(V(0, None)), Set(Fond, Inv1)),
      ChInv(Seventh(V(0, None)), Set(Fond, Inv1, Inv2, Inv3))),
    List(ChInv(Triad(I(0, None)), Set(Fond, Inv1, Inv2)),
      ChInv(Triad(III(0, None)), Set(Fond, Inv1)),
      ChInv(Triad(VI(0, None)), Set(Fond, Inv1))),
    List(ChInv(Triad(I(0, None)), Set(Fond, Inv1, Inv2)),
      ChInv(Triad(IV(0, None)), Set(Fond, Inv1)),
      ChInv(Triad(VI(0, None)), Set(Fond, Inv1))),
    List(ChInv(Triad(II(0, None)), Set(Fond, Inv1)),
      ChInv(Triad(IV(0, None)), Set(Fond, Inv1)),
      ChInv(Triad(VII(0, None)), Set(Fond, Inv1)),
      ChInv(Seventh(V(0, None)), Set(Fond, Inv1, Inv2, Inv3))),
    List(ChInv(Triad(I(0, None)), Set(Fond, Inv1, Inv2)),
      ChInv(Triad(III(0, None)), Set(Fond, Inv1)),
      ChInv(Triad(V(0, None)), Set(Fond, Inv1)),
      ChInv(Seventh(V(0, None)), Set(Fond, Inv1, Inv2, Inv3))),
    List(ChInv(Triad(I(0, None)), Set(Fond, Inv1, Inv2)),
      ChInv(Triad(III(0, None)), Set(Fond, Inv1)),
      ChInv(Triad(V(0, None)), Set(Fond, Inv1)),
      ChInv(Seventh(V(0, None)), Set(Fond, Inv1, Inv2, Inv3))),
    List(ChInv(Triad(I(0, None)), Set(Fond, Inv1, Inv2)),
      ChInv(Triad(IV(0, None)), Set(Fond, Inv1)),
      ChInv(Triad(VI(0, None)), Set(Fond, Inv1))))
  */
  val chosenChords = d.findChords(possibleChords, EndReal)

  //println(chosenChords)

  val chosenTonesL = d.findAllTones(chosenChords, melT, 4)

  println(chosenTonesL)

  /* One example : seems to work
  List(
    ChInv(EmptyChord, List()),
    ChInv(Triad(I(0, None)), Set(Inv2)),
    ChInv(Triad(I(0, None)), Set(Fond, Inv1)),
    ChInv(Triad(IV(0, None)), Set(Fond, Inv1)),
    ChInv(Triad(VII(0, None)), Set(Fond, Inv1)),
    ChInv(Triad(I(0, None)), Set(Inv2)),
    ChInv(Triad(I(0, None)), Set(Inv2)),
    ChInv(Triad(I(0, None)), Set(Fond)))
  List(
    List(O, O, O, O),
    List(V(0, None), I(1, None), III(1, None), V(1, None)),
    List(I(0, None), III(0, None), V(0, None), I(1, None)),
    List(IV(0, None), VI(0, None), I(1, None), IV(1, None)),
    List(VII(0, None), II(1, None), IV(1, None), VII(1, None)),
    List(V(0, None), I(1, None), III(1, None), V(1, None)),
    List(V(0, None), I(1, None), III(1, None), V(1, None)),
    List(I(0, None), III(0, None), V(0, None), I(1, None)))
  */

  val chosenNotes = d.tonesToNotes(chosenTonesL, mel.notes)

  val tr = d.transpose(chosenNotes)
  val end = d.createPar(tr)

  test("possChords") {
    val tpo = { O + I + II + III + IV + V + VI + VII }
    val poss = (tpo.notes map (_.tone)) map (d.getPossChords(_))
    val exp = List(List(ChInv(EmptyChord, Set.empty)),
      List(ChInv(Triad(I(0, None)), Set(Fond, Inv1, Inv2)),
        ChInv(Triad(IV(0, None)), Set(Fond, Inv1)),
        ChInv(Triad(VI(0, None)), Set(Fond, Inv1))),
      List(ChInv(Triad(II(0, None)), Set(Fond, Inv1)),
        ChInv(Triad(V(0, None)), Set(Fond, Inv1)),
        ChInv(Triad(VII(0, None)), Set(Fond, Inv1)),
        ChInv(Seventh(V(0, None)), Set(Fond, Inv1, Inv2, Inv3))),
      List(ChInv(Triad(I(0, None)), Set(Fond, Inv1, Inv2)),
        ChInv(Triad(III(0, None)), Set(Fond, Inv1)),
        ChInv(Triad(VI(0, None)), Set(Fond, Inv1))),
      List(ChInv(Triad(II(0, None)), Set(Fond, Inv1)),
        ChInv(Triad(IV(0, None)), Set(Fond, Inv1)),
        ChInv(Triad(VII(0, None)), Set(Fond, Inv1)),
        ChInv(Seventh(V(0, None)), Set(Fond, Inv1, Inv2, Inv3))),
      List(ChInv(Triad(I(0, None)), Set(Fond, Inv1, Inv2)),
        ChInv(Triad(III(0, None)), Set(Fond, Inv1)),
        ChInv(Triad(V(0, None)), Set(Fond, Inv1)),
        ChInv(Seventh(V(0, None)), Set(Fond, Inv1, Inv2, Inv3))),
      List(ChInv(Triad(II(0, None)), Set(Fond, Inv1)),
        ChInv(Triad(IV(0, None)), Set(Fond, Inv1)),
        ChInv(Triad(VI(0, None)), Set(Fond, Inv1))),
      List(ChInv(Triad(III(0, None)), Set(Fond, Inv1)),
        ChInv(Triad(V(0, None)), Set(Fond, Inv1)),
        ChInv(Triad(VII(0, None)), Set(Fond, Inv1)),
        ChInv(Seventh(V(0, None)), Set(Fond, Inv1, Inv2, Inv3))))
    assert(poss.head == List(ChInv(EmptyChord, Set.empty)))
    assert(poss == exp)
  }

  test("findAllTones fond") {
    val chosenCh = List(ChInv(EmptyChord, Set()),
      ChInv(Triad(I(0, None)), Set(Fond)),
      ChInv(Triad(II(0, None)), Set(Fond)),
      ChInv(Triad(III(0, None)), Set(Fond)),
      ChInv(Triad(IV(0, None)), Set(Fond)),
      ChInv(Triad(V(0, None)), Set(Fond)),
      ChInv(Triad(VI(0, None)), Set(Fond)),
      ChInv(Triad(VII(0, None)), Set(Fond)),
      ChInv(Seventh(V(0, None)), Set(Fond)))
    val at1 = d.findAllTones(chosenCh, melT ::: List(V), 4)
    val exp = List(
      List(O, O, O, O),
      List(I(0, None), III(0, None), V(0, None), I(1, None)),
      List(II(0, None), IV(0, None), VI(0, None), II(1, None)),
      List(III(0, None), V(0, None), VII(0, None), III(1, None)),
      List(IV(0, None), VI(0, None), I(1, None), IV(1, None)),
      List(V(0, None), VII(0, None), II(1, None), V(1, None)),
      List(VI(0, None), I(1, None), III(1, None), VI(1, None)),
      List(VII(0, None), II(1, None), IV(1, None), VII(1, None)),
      List(V(0, None), VII(0, None), II(1, None), IV(1, None)))
    assert(at1 == exp)
  }

  test("findAllTones inv") {
    val chosenCh = List(
      ChInv(Triad(I(0, None)), Set(Fond)),
      ChInv(Triad(I(0, None)), Set(Inv1)),
      ChInv(Triad(I(0, None)), Set(Inv2)),
      ChInv(Triad(I(0, None)), Set(Inv3)),
      ChInv(Seventh(V(0, None)), Set(Fond)),
      ChInv(Seventh(V(0, None)), Set(Inv1)),
      ChInv(Seventh(V(0, None)), Set(Inv2)),
      ChInv(Seventh(V(0, None)), Set(Inv3)))
    val at1 = d.findAllTones(chosenCh, melT, 4)
    val exp = List(
      List(I(0, None), III(0, None), V(0, None), I(1, None)),
      List(III(0, None), V(0, None), I(1, None), III(1, None)),
      List(V(0, None), I(1, None), III(1, None), V(1, None)),
      List(I(0, None), III(0, None), V(0, None), I(1, None)),
      List(V(0, None), VII(0, None), II(1, None), IV(1, None)),
      List(VII(0, None), II(1, None), IV(1, None), V(1, None)),

      //TODO : perhaps get the lowest possible octave always ?
      List(II(1, None), IV(1, None), V(1, None), VII(1, None)),
      List(IV(1, None), V(1, None), VII(1, None), II(2, None)))

    assert(at1 == exp)
  }
}