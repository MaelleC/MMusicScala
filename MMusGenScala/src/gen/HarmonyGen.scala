package gen

import segmentSystem._
import tonalSystem.Tone
import tonalSystem.Tone._
import scala.util.Random
import chord._
import segmentSystem.ClassPredicate._
import scala.collection.mutable.ListBuffer
import Chord._

case class HarmonyGen(melody: MusicalSegment) { //TODO : need that for test.Harm, perhaps change at the end
  val allChords: List[Chord] = List(Triad(I), Triad(II), Triad(III),
    Triad(IV), Triad(V), Triad(VI), Triad(VII), Seventh(V))
  //TODO add all what is in the grammar
  //TODO : should be ChInv perhaps ?

  val nbChordNotes = 4; //TODO : for now, for basic not placement

  def harmonize(endF: ChiEnd, useC: Boolean = false, compcorig: List[(Int, List[CConstr])] = Nil): (MusicalSegment, ParallelSegment) = {
    val mel = getMel(melody)

    //TODO for now, 2 octaves below the lowest note of the melody 
    //TODO if melody too low : put it higher (of octaves) -> then : tell back to caller !!
    //TODO or : put always same bound : I(0)
    // and put higher the melody if there is a note lower than some threshold ? -> then : tell back to caller !!
    val lowerBound = mel.notes.min(NoteOrdering) - 14 //risky if min is too low

    val melT = mel.notes map (_.tone)
    val compc: List[List[CConstr]] = {
      compcorig match {
        case Nil => Nil
        case _ => getConsList(compcorig, melT.length)
      }
    }
    val compcForEnd: Boolean = {
      if (compc.isEmpty) false
      else compc.last.head != NoCons
    }
    val possibleChords: List[List[ChInv]] = {
      if (compc.isEmpty) melT map (getPossChords(_))
      else (melT zip compc) map (getPossChordsCons(_))
    }

    val chosenChords = {
      if (!useC) findChords(possibleChords, endF)
      else findChordsC(possibleChords, endF, (compc.isEmpty || !compcForEnd))
    }
    val chosenTonesL = findAllTones(chosenChords, melT, nbChordNotes)
    val chosenNotes = tonesToNotes(chosenTonesL, mel.notes)

    (mel, createPar(transpose(chosenNotes)))

  }

  def getMel(melody: MusicalSegment): MusicalSegment = {
    val flatMel = melody.flatAll
    if (flatMel.parDepth != 0) {
      getOneVoice(flatMel)
    } else flatMel
  }

  def getConsList(ori: List[(Int, List[CConstr])], melLen: Int): List[List[CConstr]] = {
    //!!!!!!!!!!
    //TODO : verify that in possibleChords !!
    //also : put in prevPoss things for all chords of possibleChords !!!
    def noConsL(i1: Int, i2: Int): List[List[CConstr]] = (List.range(i1, i2) map { x => List(NoCons) })
    def getConsList0(buf: List[List[CConstr]], o: List[(Int, List[CConstr])]): List[List[CConstr]] = {
      if (o.isEmpty) {
        if (buf.length != melLen) buf ::: noConsL(buf.length, melLen)
        else buf
      } else {
        if (o.head._1 < buf.length) getConsList0(buf, o.tail)
        else getConsList0(buf ::: noConsL(buf.length, o.head._1) ::: List(o.head._2), o.tail)
      }
    }

    val indices = ori map { x => x._1 }
    if (indices.max >= melLen) error("Indices should be less than " + melLen + " (which is the length of the melody).")
    else if (indices.min < 0) error("Indices should be greater or equal to zero.")
    //TODO : in increasing order ?, no duplicates ? drop them in getConsList0 and give a warning
    getConsList0(Nil, ori)
  }

  def getOneVoice(mel: MusicalSegment): MusicalSegment = {
    if (mel.parDepth != 0) {
      getOneVoice(mel mapIf (isPar thenDo (_.melody.head)))
    } else mel
  }

  def getPossChords(t: Tone): List[ChInv] = {
    def possInv(c: Chord): List[Inversion] = {
      c match {
        case EmptyChord => Nil
        case Triad(I(_, _)) => List(Fond, Inv1, Inv2)

        //TODO : knowing t tone of the melody, 
        // put away some inversions (ex : if 3rd, no Inv2)
        case Triad(_) => List(Fond, Inv1)
        case Seventh(_) => List(Fond, Inv1, Inv2, Inv3)

        //TODO : add other special cases (in terms of chord)

        case _ if (c.tones.size == 3) => List(Fond, Inv1, Inv2)
        case _ if (c.tones.size == 4) => List(Fond, Inv1, Inv2, Inv3)
        case _ => Nil // TODO : perhaps not the best to do ? normally, nothing will go there
      }
    }

    t match {
      case O => List(ChInv(EmptyChord, Nil))
      case _ => (allChords.filter(_.contains(t))).map(x => ChInv(x, possInv(x)))
    }
  }

  def getPossChordsCons(cc: (Tone, List[CConstr])): List[ChInv] = ???

  //find chords with formal constraints
  //compc info is included in poss
  def findChordsC(poss: List[List[ChInv]], endF: ChI, useChi: Boolean): List[ChInv] = ???

  //find chords without formal constraints
  def findChords(poss: List[List[ChInv]], endF: ChI): List[ChInv] = {

    def findChord(endi: ChI, possC: List[List[ChInv]], buf: List[ChInv]): List[ChInv] = {
      if (possC.isEmpty) return buf
      else if (possC.head.isEmpty) {
        //bizarre note
        //TODO if bizarre note : put some triad of it,
        // or put a harmonically correct chord ? (-> dissonance) ? : if cadence, yes ?
        if (buf.isEmpty) {
          // at the end
          val nextC = ChInv(EmptyChord, Nil)
          findChord(nextC, possC.tail, nextC :: buf)
        } else {
          //keep the previous chord
          val nextC = buf.head
          findChord(nextC, possC.tail, nextC :: buf)
        }
      } else if (possC.head.head.c == EmptyChord) {
        //silent : give silent chord, but continues harmony for next
        findChord(endi, possC.tail, ChInv(EmptyChord, Nil) :: buf)
      }

      val possChI = possC.head
      val inter: List[ChInv] = {
        if (endi == NoEnd) possChI
        else intersectChInv(possChI, prevPoss(endi))
      }
      if (inter.isEmpty) {
        //no possible chord is harmonically ok,
        // but there are possible chords (possC.head isn't empty)
        if (buf.nonEmpty && intersectChInv(possChI, List(buf.head)).nonEmpty) {
          //give the previous if it is possible
          val nextC = intersectChInv(possChI, List(buf.head)).head
          findChord(nextC, possC.tail, nextC :: buf)
        } else {
          //take a possible chord, even if harmonically not ok
          val nextC = Random.shuffle(possChI).head
          findChord(nextC, possC.tail, nextC :: buf)
        }

      } else {
        //normal case : random between ok chords
        val nextC = Random.shuffle(inter).head
        findChord(nextC, possC.tail, nextC :: buf)
      }

    }

    def intersectChInv(a: List[ChInv], b: List[ChInv]): List[ChInv] = {
      val ca = a.map(_.c)
      val cb = b.map(_.c)
      val cint = ca.intersect(cb)

      cint map { x => ChInv(x, (a.find(c => c.c == x).get.i).intersect(b.find(c => c.c == x).get.i)) }

    }

    findChord(endF, poss.reverse, Nil)
  }

  /**
   * returns a sequential list of parallel list of notes, without the melody
   */
  def findAllTones(chords: List[ChInv], mel: List[Tone], nbNotes: Int /*, lowerBound: Tone*/ ): List[List[Tone]] = {
    //TODO ? : fct that put away some inversions from a list of chord
    // (ex : if V7+ -> I, I has to be Fond, can't be I6)

    def findTones(ch: ChInv, melN: Tone): List[Tone] = {
      //TODO ? : for now, gives the nbNotes first notes of each chord
      //later : take melody into account, esp. if nbNotes = 3 for Seventh, esp. if inversions
      //also : sometimes use Inv1 or inversions of seventh
      ch.c match {
        case EmptyChord => (0 until nbNotes).toList map { y => O }
        case Seventh(t) =>
          (0 until nbNotes).toList map { y => ch.c(y + (ch.i.head.first % ch.c.tones.length)) }
        case _ if (nbNotes != 4) =>
          (0 until nbNotes).toList map { y => ch.c(y + (ch.i.head.first % ch.c.tones.length)) }
        case Triad(t) => ch.i.head match {
          case Fond | Inv3 => (0 until nbNotes).toList map { y => ch.c(y) }
          //it should sound better like that
          case Inv1 =>
            List(1, 2, 3, 5) map { y => ch.c(y) } //or 1356
          case Inv2 =>
            List(2, 3, 4, 6) map { y => ch.c(y) }
        }
        case _ => (0 until nbNotes).toList map { y => O } //TODO : something else ?
      }
    }

    (chords zip mel) map { x =>
      {
        val re = findTones(x._1, x._2)
        //if lower note has octave >= 1 : lower all
        if (re.head.octave >= 1) {
          re map { y => y.newTone(y.octave - re.head.octave, y.alter) }
        } else {
          re
        }
      }
    }
  }

  // from http://stackoverflow.com/questions
  // /1683312/is-there-a-safe-way-in-scala-to-transpose-a-list-of-unequal-length-lists
  def transpose[A](xss: List[List[A]]): List[List[A]] = {
    val buf = new ListBuffer[List[A]]
    var yss = xss
    while (!yss.head.isEmpty) {
      buf += (yss map (_.head))
      yss = (yss map (_.tail))
    }
    buf.toList
  }
  def tonesToNotes(tones: List[List[Tone]], notes: List[Note]): List[List[Note]] = {
    (tones zip notes) map (x => x._1 map (y => Note(y, x._2.duration)))
  }

  def createPar(voices: List[List[Note]]): ParallelSegment = {
    def toSequ(voice: List[Note]): SequentialSegment = {
      voice.foldLeft[SequentialSegment](EmptySeq)((s, n) => s + n)
    }
    voices.tail.foldLeft(EmptyPar | toSequ(voices.head))((x, y) => x | toSequ(y))
  }

  /*
  def chooseOneIn[A](l: List[A]): A = {
    Random.shuffle(l).head
  }
  */

  //TODO : way to give a list of possible chords from a grammar (for allPoss of HarmonyGen ?)
  //-- then : define a trait of harmonyGenerizers !!
  def prevPoss(ci: ChI) /* extends PartialFunction[??]*/ : List[ChInv] = {
    ci match {
      case ChInv(Triad(I(_, None)), i) if testInv(i) => ChInv(Seventh(V), List(Fond, Inv1, Inv2, Inv3)) :: HarmFct(V)
      case ChInv(Triad(II(_, None)), i) if testInv(i) => HarmFct(I)
      //case ChInv(Triad(III(_, None)), i) if testInv(i) => ??? //no need yet
      case ChInv(Triad(IV(_, None)), i) if testInv(i) => HarmFct(I)
      case ChInv(Triad(V(_, None)), i) if testInv(i) => HarmFct(I) ::: HarmFct(IV)
      case ChInv(Triad(VI(_, None)), i) if testInv(i) => getCiL(V, i)
      case ChInv(Triad(VII(_, None)), i) if testInv(i) => HarmFct(I) ::: HarmFct(IV)
      case ChInv(Triad(I(_, None)), i) if testInv(i, List(Inv2)) => HarmFct(IV) //TODO ? in fact : II in Inv 1 only
      case ChInv(Seventh(V(o, None)), i) if testInv(i) => prevPoss(ChInv(Triad(V(o, None)), List(Fond, Inv1)))
      //TODO : add others ? (perhaps no need yet of nap and secondaryDoms)
      case EndReal => getCiL(I, List(Fond)) ::: getCiL(V, List(Fond))
      case EndMiddle => getCiL(I, List(Fond)) ::: getCiL(VI, List(Fond)) //perhaps Inv1 ok for VI ?
      case EndHalf => getCiL(V, List(Fond))
      case NoEnd => error("no NoEnd should go into prevPoss") //TODO : perhaps manage differently
      case _ => Nil
    }
  }

  //TODO perhaps better way to do pattern matching with list
  def testInv(i: List[Inversion], li: List[Inversion] = List(Fond, Inv1)): Boolean = {
    i.filter(x => li.contains(x)).nonEmpty
  }

  def HarmFct(t: Tone): List[ChInv] = {
    t match {
      case I(_, None) => List(I, VI).map(x => ChInv(Triad(x), List(Fond, Inv1)))
      case V(_, None) => ChInv(Triad(I), List(Inv2)) :: //I64
        List(V, VII).map(x => ChInv(Triad(x), List(Fond, Inv1)))
      case IV(_, None) => List(IV, II).map(x => ChInv(Triad(x), List(Fond, Inv1)))
      case _ => Nil
    }
  }

  def getCiL(t: Tone, i: List[Inversion]): List[ChInv] = List(t).map(x => ChInv(Triad(x), i))

  /*
  def followPoss(c: Chord) /* extends PartialFunction[??]*/ : List[List[Chord]] = {
    ???
  }*/

}




