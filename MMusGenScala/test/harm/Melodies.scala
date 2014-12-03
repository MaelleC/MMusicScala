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

object Melodies extends App with MelodyWriter{
  implicit val noteDuration = E
	val parttest = {
    I + I + II + II + III + III + IV + IV + III + III + II + II + I/0.5
  }
  val partharm1 = {
    (I/0.5  + III  + IV)/0.5
  }
  val partharm2 = {
    (III +  II  + I)/0.5
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