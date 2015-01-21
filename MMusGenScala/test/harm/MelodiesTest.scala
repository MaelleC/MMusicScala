package harm

import midiInterface.MelodyPlayer
import rythmics._
import utils.MelodyWriter
import tonalSystem.C
import tonalSystem.A
import tonalSystem.Tone._
import tonalSystem.Major
import tonalSystem.Minor
import tonalSystem.Scale
import segmentSystem._
import gen._
import org.scalatest.FunSuite
import chord._
import gen.Chord._ //implicit conversion

class MelodiesTest extends FunSuite with MelodyWriter {
  implicit val noteDuration = E

  val tempo = 60
  val instrument = 54
  val minscale = Minor(A)
  val majscale = Major(C)

  def playMel(mel: MusicalSegment, acc: MusicalSegment, s: Scale): Unit = {
    MelodyPlayer((mel + 14 | acc) withScale majscale, tempo, instrument = instrument)
  }

  test("oneMelody") {
    val parttest = {
      (V - 7) + I + I + II + II + III + III + IV + IV + III + III + II + II + I / 0.5
    }
    val partharm1 = {
      O + ((I / 0.5 + III + IV) / 0.5)
    }
    val partharm2 = {
      (III + II + I) / 0.5
    }

    val d = HarmonyGen(partharm1)
    //    val f = HarmonyGen(partharm2)
    //    val e = parttest | (d.harmonize(EndHalf)._2 + f.harmonize(EndReal)._2)
    val f = HarmonyGen((partharm1 + partharm2))
    val e = parttest + 14 | ((f.harmonize(EndReal, true)._2))
    playMel(parttest, f.harmonize(EndReal, true)._2, majscale)
  }

  test("clair de la lune") { //constraints : 1 ok, 2 not
    val mel = {
      I + I + I + II + III / (0.5) + II / (0.5) + I + III + II + II + I / (0.25) +
        II + II + II + II + (VI - 7) / (0.5) + (VI - 7) / (0.5) + II + I + (VII - 7) + (VI - 7) + (V - 7) / (0.25) +
        I + I + I + II + III / (0.5) + II / (0.5) + I + III + II + II + I / (0.25)
    }

    val harm1 = {
      I / (0.25) + III / (0.5) + II / (0.5) + I / (0.5) + II / (0.5) + I / (0.25)
    }
    val harm2 = {
      II / (0.25) + (VI - 7) / (0.5) + (VI - 7) / (0.5) + II / (0.5) + (VII - 7) / (0.5) + (V - 7) / (0.25)
    }

    val p1 = HarmonyGen(harm1)
    val p2 = HarmonyGen(harm2)
    val part1h = p1.harmonize(EndReal)._2
    val part2h = p2.harmonize(EndHalf)._2

    playMel(mel, part1h + part2h + part1h, majscale)
  }

  test("il est ne") { //constraints : parts 2 and 3 ok
    val mel = {
      (V - 7) + I + I + III / (2) + I / (2) + (V - 7) + I + I / (0.5) +
        I + I / (2) + II / (2) + III + IV / (2) + III / (2) + II + I + II + II +
        (V - 7) + I + I + III / (2) + I / (2) + (V - 7) + I + I / (0.5) +
        I + II + III + IV / (2) + III / (2) + II + V + I / (0.5)
    }

    val harm1 = {
      (V - 7) + I + I + III / (2) + I / (2) + (V - 7) + I + I / (0.5)
    }
    val harm2 = {
      I + I / (2) + II / (2) + III + IV / (2) + III / (2) + II + I + II + II
    }

    val harm3 = {
      I + II + III + IV / (2) + III / (2) + II + V + I / (0.5)
    }

    val p1 = HarmonyGen(harm1)
    val p2 = HarmonyGen(harm2)
    val p3 = HarmonyGen(harm3)
    val part1h = p1.harmonize(EndReal)._2
    val part2h = p2.harmonize(EndHalf)._2
    val part3h = p3.harmonize(EndReal)._2

    playMel(mel, part1h + part2h + part1h + part3h, majscale)

  }

  test("il etait un avocat - all harm") { //with constraints ok
    val withConstraints = true
    val mel = {
      I / (2) + III / (2) + I / (2) + III / (2) + I / (2) + III / (2) + V +
        V / (4 / 3.0) + IV / (4) + III / (2) + II / (2) + III + I +
        I / (2) + III / (2) + I / (2) + III / (2) + I / (2) + III / (2) + V +
        V / (4 / 3.0) + IV / (4) + III / (2) + II / (2) + I / (0.5)
    }

    val harm1 = {
      I / (2) + III / (2) + I / (2) + III / (2) + I / (2) + III / (2) + V
    }
    val harm2 = {
      V / (4 / 3.0) + IV / (4) + III / (2) + II / (2) + III + I
    }

    val harm3 = {
      V / (4 / 3.0) + IV / (4) + III / (2) + II / (2) + I / (0.5)
    }

    val p1 = HarmonyGen(harm1)
    val p2 = HarmonyGen(harm2)
    val p3 = HarmonyGen(harm3)
    val part1h = p1.harmonize(EndReal, withConstraints)._2
    val part2h = p2.harmonize(EndHalf, withConstraints)._2
    val part3h = p3.harmonize(EndReal, withConstraints)._2

    playMel(mel, part1h + part2h + part1h + part3h, majscale)

  }

  test("il etait un avocat - not all harm") { //with constraints ok
    val withConstraints = true
    val mel = {
      I / (2) + III / (2) + I / (2) + III / (2) + I / (2) + III / (2) + V +
        V / (4 / 3.0) + IV / (4) + III / (2) + II / (2) + III + I +
        I / (2) + III / (2) + I / (2) + III / (2) + I / (2) + III / (2) + V +
        V / (4 / 3.0) + IV / (4) + III / (2) + II / (2) + I / (0.5)
    }

    val harm1 = {
      I + I + I + V
    }
    val harm2 = {
      V + III + III + I
    }

    val harm3 = {
      V + III / (2) + II / (2) + I / (0.5)
    }

    val p1 = HarmonyGen(harm1)
    val p2 = HarmonyGen(harm2)
    val p3 = HarmonyGen(harm3)
    val part1h = p1.harmonize(EndReal, withConstraints)._2
    val part2h = p2.harmonize(EndHalf, withConstraints)._2
    val part3h = p3.harmonize(EndReal, withConstraints)._2

    playMel(mel, part1h + part2h + part1h + part3h, majscale)

  }

}