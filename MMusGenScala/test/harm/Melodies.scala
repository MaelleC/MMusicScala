package harm

import midiInterface.MelodyPlayer
import rythmics._
import utils.MelodyWriter
import tonalSystem.A
import tonalSystem.Tone._
import tonalSystem.Major
import tonalSystem.Minor
import segmentSystem.Sequential
import gen._
import org.scalatest.FunSuite

class Melodies extends FunSuite with MelodyWriter {
  implicit val noteDuration = E

  test("oneMelody") {
    val parttest = {
      I + I + II + II + III + III + IV + IV + III + III + II + II + I / 0.5
    }
    val partharm1 = {
      (I / 0.5 + III + IV) / 0.5
    }
    val partharm2 = {
      (III + II + I) / 0.5
    }

    val tempo = 60
    val instrument = 0
    val minScale = Minor(A)

    val d = HarmonyGen(partharm1)
    val f = HarmonyGen(partharm2)
    val e = parttest | (d.harmonize(EndHalf)._2 + f.harmonize(EndReal)._2)

    MelodyPlayer(
      Sequential(Nil)
        + (e withScale Major(A)),
      tempo,
      instrument = instrument)
  }
  
  test("oneMelodyCons") {

    val partharm = {
      I + IV + V + I
    }
 

    val tempo = 60
    val instrument = 0
    val minScale = Minor(A)

    val d = HarmonyGen(partharm)
    val e = d.harmonize(EndReal, true)._2

    MelodyPlayer(
      Sequential(Nil)
        + (e withScale Major(A)),
      tempo,
      instrument = instrument)
  }

}