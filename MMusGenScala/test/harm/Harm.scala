package harm
import org.scalatest.FunSuite
import tonalSystem.Tone._
import segmentSystem._
import utils.MelodyWriter
import rythmics._
import gen._
import midiInterface.MelodyPlayer

class Harm extends FunSuite with MelodyWriter {
  implicit val noteDuration = E
  
  val part1 = {
    ( O + III + IV
	+ V/(2/3.0) + IV/(2.0) + III
	+ III + I + (VII -7) 
	+ (VII-7)/2.0 + I/(2/3.0) + III
	+ IV(Q) + III(S) + IV(S) 
	+ V/(2/3.0) + VI/(2.0) + VII 
	+ V + III + II 
	+ I/(1/4.0))
  }
  
  val part1harm = {
	( O 
	+ V 
	+ III 
	+ I
	+ IV 
	+ V
	+ V
	+ I/(1/2.0)) /(1/3.0)
  }
  
  val d = HarmonyGen(part1harm)
  
  test("aaa") {
    assert(1 == 1)
  }
  
  test("aa") {
    assert(1 == 0)
  }
}