package play

import midiInterface.MelodyPlayer
import rythmics._
import segmentSystem.MusicalSegment
import segmentSystem.SequentialSegment
import tonalSystem.A
import tonalSystem.Tone._
import tonalSystem.Major
import tonalSystem.Minor
import utils.SS
import utils.Print
import utils.PrettyPrinter
import utils.MelodyWriter
import segmentSystem.Sequential
import segmentSystem.ClassPredicate
import gen._
import chord._
import gen.Chord._ //implicit conversion

object Example extends App with MelodyWriter {
  implicit val noteDuration = H

  val part = {
    IV / (4) + VI / (4) + I(1) / (4) + VI / (4) +
      V / (2) + III / (4) + II / (4) +
      I / (4) + I(1) / (4) + II(1) / (4) + I(1) / (4) +
      VII / (2) + V / (4) + VI / (4) + //
      VII / (4) + V / (4) + II / (2) +
      VI / (2) + I / (4) + VII / (4) +
      V / (2) + VII / (4) + V / (4) +
      IV(0, Some(true)) / (4) + II / (4) + III / (4) + IV(0, Some(true)) / (4) +
      V +
      I(1) / (2) + VII / (4) + I(1) / (4) +
      II(1) / (4) + III(1) / (4) + IV(1) / (2) +
      II(1) / (2) + VII / (2) +
      V / (2) + I(1) / (4) + VII / (4) +
      VI / (2) + V / (2) +
      IV / (2) + III / (4) + II / (4) +
      V / (4) + VII / (4) + II(1) / (4) + IV(1) / (4) +
      III(1) / (2) + II(1) / (2) +
      I(1)
  }

  val part1Harm = {
    IV + V + I + VII + VII + VI + V + IV(0, Some(true)) + V + I + II + II + V + VI + IV + V + III + I
    //linear nearly always tell : problem with note 15 :
    //warn : note 15 : not chord harmonically ok is compatible with the note.
    //the previous chord is kept.
  }
  //we will fusion it with I, since at least Triad(I) contains III

  val part2Harm = {
    IV + V + I + VII + VII + VI + V + IV(0, Some(true)) + V + I + II + II + V + VI + IV + V + I / (0.5)
    //linear : nearly always tells : problem with note 10 :
    //warn : note 10 : not chord harmonically ok is compatible with the note.
    //the previous chord is kept.
  }

  //so we fusion the double II
  val part3Harm = {
    IV + V + I + VII + VII + VI + V + IV(0, Some(true)) + V + I + II / (0.5) + V + VI + IV + V + I /(0.5)
    //linear tells : always problem with note 7 :
    //warn : note 7 : no chord compatible with the note, too bizarre note.
    //previous chord is kept.
  }
  // so we fusion the two V with the IV that has an accidental

  val part4Harm = {
    IV + V + I + VII + VII + VI + V  / (1/3.0) + I + II / (0.5) + V + VI + IV + V + I /(0.5)
    //linear tells : note 3 not problem (sometimes with note 4, notes 5, note 8, but nearly always 3 is there)
    //warn : note 3 : not chord harmonically ok is compatible with the note.
    //	the previous chord is kept.
  }
  //so we fusion note 3 with note 4

  val partFinalHarm = {
    IV + V + I + VII / (0.5) + VI + V / (1/3.0) + I + II / (0.5) + V + VI + IV + V + I /(0.5)
  }

  val tempo = 100
  val instrument = 0
  val minScale = Major(A)

//  val h1 = HarmonyGen(part1Harm)
//  val mlin = (part + 7) | h1.harmonize(EndReal, false)._2 //one example is on record "linearHarmonizer_Begin.mid"
//  val m1 = (part + 7) | h1.harmonize(EndReal, true)._2
//
//  val h2 = HarmonyGen(part2Harm)
//  val m2 = (part + 7) | h2.harmonize(EndReal, true)._2
//
//  val h3 = HarmonyGen(part3Harm)
//  val m3 = (part + 7) | h3.harmonize(EndReal, true)._2
//
//  val h4 = HarmonyGen(part4Harm)
//  val m4 = (part + 7) | h4.harmonize(EndReal, true)._2
//  
  val hf = HarmonyGen(partFinalHarm)
  val mf = (part + 7) | hf.harmonize(EndReal, true)._2 //one example is on record "constraintsBasedHarmonizer.mid"
//  val lf = (part + 7) | hf.harmonize(EndReal, false)._2 //one example is on record "linearHarmonizer_Begin.mid"
  
  MelodyPlayer(
    Sequential(Nil)
      + (mf withScale minScale),
    tempo,
    instrument = instrument)

}