package gen

import segmentSystem._
import tonalSystem.Tone
import tonalSystem.Tone._
import scala.util.Random
import chord._
import segmentSystem.ClassPredicate._
import scala.collection.mutable.ListBuffer
import Chord._
import cafesat.api.API._

case class HarmonyGen(melody: MusicalSegment) { //TODO : need that for test.Harm, perhaps change at the end
  val allChords: List[Chord] = List(Triad(I), Triad(II), Triad(III),
    Triad(IV), Triad(V), Triad(VI), Triad(VII), Seventh(V))
  //TODO add all what is in the grammar
  //TODO : should be ChInvPoss perhaps ?

  val nbChordNotes = 4; //TODO : for now, for basic note placement

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
      //TODO perhaps different when several only one chord ok from composer
      else (melT zip compc) map (getPossChordsCons(_))

    }

    //TODO : for Triad(I) : separate in two ChInvs : One with Fond and In1, one with Inv2
    //and change prevPoss
    //perhaps always put only one inversion in each chInv ?

    val chosenChords = {
      if (!useC) findChords(possibleChords, endF)
      else findChordsC(possibleChords, endF, (compc.isEmpty || !compcForEnd)) match {
        case Some(possC) => possC
        case None => { //TODO : differently ?
          println("could not solve with constraints, try without, but not all composer constraints will be satisfied.")
          findChords(possibleChords, endF)
        }
      }
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
    def possChInv(c: Chord): List[ChInv] = {
      c match {
        case EmptyChord => List(ChInv(EmptyChord, Fond))
        case Triad(I(_, _)) => List(ChInv(c, Fond), ChInv(c, Inv1), ChInv(c, Inv2))

        //TODO : knowing t tone of the melody, 
        // put away some inversions (ex : if 3rd, no Inv2)
        case Triad(_) => List(ChInv(c, Fond), ChInv(c, Inv1))
        case Seventh(_) => List(ChInv(c, Fond), ChInv(c, Inv1), ChInv(c, Inv2), ChInv(c, Inv3))

        //TODO : add other special cases (in terms of chord)

        case _ if (c.tones.size == 3) => List(ChInv(c, Fond), ChInv(c, Inv1), ChInv(c, Inv2))
        case _ if (c.tones.size == 4) => List(ChInv(c, Fond), ChInv(c, Inv1), ChInv(c, Inv2), ChInv(c, Inv3))
        case _ => List(ChInv(c, Fond)) // TODO : perhaps not the best to do ? normally, nothing will go there
      }
    }

    t match {
      case O => List(ChInv(EmptyChord, Fond))
      case _ => (allChords.filter(_.contains(t))).map(x => possChInv(x)).flatten
    }
  }

  def getPossChordsCons(cc: (Tone, List[CConstr])): List[ChInv] = {
    def mergeChInv(p: List[ChInv], c: List[CConstr]): List[ChInv] = {
      if (c.head == NoCons) p
      else {
        val chInvs = (c map { x => possInvToInv(x) }).flatten
        for { i <- p; j <- chInvs if i == j }
          yield i
      }
    }
    mergeChInv(getPossChords(cc._1), cc._2)
  }

  def possInvToInv(cip: CConstr): List[ChInv] = {
    cip match {
      case NoCons => Nil
      case ChInvPoss(c, l) => l.toList map { x => ChInv(c, x) }
    }
  }

  //find chords with formal constraints
  //compc info is included in poss
  def findChordsC(poss: List[List[ChInv]], endF: HavePrev, useChi: Boolean): Option[List[ChInv]] = {

    //TODO : should have one variable for each possible inversion at least for Triad(I), 
    //otherwise : problem for prevPoss when diff (do the thing in "harmonize")

    val consVarsCh: List[List[(ChInv, Formula)]] = poss map { x => x map { y => (y, boolVar()) } }
    val onlyOneChInv = ((consVarsCh map { x => x map { y => y._2 } }) map { x => Constraints.exactlyOne(x) }).flatten

    def possPairs(c1: List[(ChInv, Formula)], c2: List[(ChInv, Formula)]): List[(Formula, Formula)] = {
      (c2 map { x => mergeP(c1, (prevPoss(x._1), x._2)) }).flatten
    }
    def mergeP(prev: List[(ChInv, Formula)], poss: (List[ChInv], Formula)): List[(Formula, Formula)] = {
      for (i <- poss._1; j <- prev if j == i) yield (j._2, poss._2)
      //TODO test if really ok the equality
      //ok if sam inv set, but if subset : consider ok or not ?
    }

    val pairsFormulas = (consVarsCh zip consVarsCh.tail) map { x => possPairs(x._1, x._2) }

    val pairVars = pairsFormulas map { x => x map { y => boolVar() } }
    val onlyOnePairChInv = ((pairsFormulas zip pairVars) map { x => Constraints.exactlyOnePair(x._1, x._2) }).flatten

    val allConstraints = onlyOneChInv ++ onlyOnePairChInv
    solveForSatisfiability(and(allConstraints: _*)) match {
      case None => None
      case Some(result) => Some(consVarsCh map { x => (x.filter { y => result(y._2) }).head._1 })
    }
  }

  //find chords without formal constraints
  def findChords(poss: List[List[ChInv]], endF: HavePrev): List[ChInv] = {

    //    def findEnd(endi: ChiEnd, possC: List[List[ChInv]]): List[ChInv] = {
    //      findChord(, possC.tail, )
    //      //TOD
    //    }

    def findChord(next: HavePrev, possC: List[List[ChInv]], buf: List[ChInv]): List[ChInv] = {
      if (possC.isEmpty) return buf
      else if (possC.head.isEmpty) {
        //bizarre note
        //TODO if bizarre note : put some triad of it,
        // or put a harmonically correct chord ? (-> dissonance) ? : if cadence, yes ?
        if (buf.isEmpty) {
          // at the end
          val nextC = ChInv(EmptyChord, Fond)
          findChord(nextC, possC.tail, nextC :: buf)
        } else {
          //keep the previous chord
          val nextC = buf.head
          findChord(nextC, possC.tail, nextC :: buf)
        }
      } else if (possC.head.head.c == EmptyChord) {
        //silent : give silent chord, but continues harmony for next
        findChord(next, possC.tail, ChInv(EmptyChord, Fond) :: buf)
      }

      val possChI = possC.head
      val inter: List[ChInv] = {
        if (next == NoEnd) possChI
        else possChI.intersect(prevPoss(next))
      }
      if (inter.isEmpty) {
        //no possible chord is harmonically ok,
        // but there are possible chords (possC.head isn't empty)
        if (buf.nonEmpty && possChI.intersect(List(buf.head)).nonEmpty) {
          //give the previous if it is possible
          val nextC = possChI.intersect(List(buf.head)).head
          findChord(nextC, possC.tail, nextC :: buf)
        } else {
          //take a possible chord, even if harmonically not ok

          val nextC = (new Random(78)).shuffle(possChI).head
          findChord(nextC, possC.tail, nextC :: buf)
        }

      } else {
        //normal case : random between ok chords
        val nextC = Random.shuffle(inter).head
        findChord(nextC, possC.tail, nextC :: buf)
      }

    }

    //findEnd(endF, poss.reverse)
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
          (0 until nbNotes).toList map { y => ch.c(y + (ch.i.first % ch.c.tones.length)) }
        case _ if (nbNotes != 4) =>
          (0 until nbNotes).toList map { y => ch.c(y + (ch.i.first % ch.c.tones.length)) }
        case Triad(t) => {
          ch.i match {
            case Fond | Inv3 => (0 until nbNotes).toList map { y => ch.c(y) }
            //it should sound better like that
            case Inv1 =>
              List(1, 2, 3, 5) map { y => ch.c(y) } //or 1356
            case Inv2 =>
              List(2, 3, 4, 6) map { y => ch.c(y) }
          }
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
  //TODO : relax some constraints (fi1 really necessary always ?)
  def prevPoss(ci: HavePrev) /* extends PartialFunction[??]*/ : List[ChInv] = {
    ci match {
      case ChInv(Triad(I(_, None)), i) if fi1(i) => possInvToInv(ChInvPoss(Seventh(V), Set(Fond, Inv1, Inv2, Inv3))) ::: HarmFct(V)
      case ChInv(Triad(II(_, None)), i) if fi1(i) => HarmFct(I)
      //case ChInv(Triad(III(_, None)), i) if fi1(i) => ??? //no need yet
      case ChInv(Triad(IV(_, None)), i) if fi1(i) => HarmFct(I)
      case ChInv(Triad(V(_, None)), i) if fi1(i) => HarmFct(I) ::: HarmFct(IV)
      case ChInv(Triad(VI(_, None)), i) if fi1(i) => getCiL(List(V), List(Fond, Inv1))
      case ChInv(Triad(VII(_, None)), i) if fi1(i) => HarmFct(I) ::: HarmFct(IV)
      case ChInv(Triad(I(_, None)), Inv2) => HarmFct(IV) //TODO ? in fact : II in Inv 1 only
      case ChInv(Seventh(V(o, None)), i) => prevPoss(ChInv(Triad(V(o, None)), Fond)) //TODO : verify inversion ?
      //TODO : add others ? (perhaps no need yet of nap and secondaryDoms)
      case EndReal => List(ChInv(Triad(I), Fond))
      case EndMiddle => ChInv(Triad(I), Fond) :: getCiL(List(VI), List(Fond, Inv1))
      case EndHalf => List(ChInv(Triad(V), Fond))
      case NoEnd => error("no NoEnd should go into prevPoss") //TODO : perhaps manage differently
      case _ => Nil
    }
  }
  def fi1(i: Inversion): Boolean = {
    i == Fond || i == Inv1
  }

  def HarmFct(t: Tone): List[ChInv] = {
    t match {
      case I(_, None) => getCiL(List(I, VI), List(Fond, Inv1))
      case V(_, None) => ChInv(Triad(I), Inv2) :: //I64
        getCiL(List(V, VII), List(Fond, Inv1))
      case IV(_, None) => getCiL(List(IV, II), List(Fond, Inv1))
      case _ => Nil
    }
  }

  def getCiL(t: List[Tone], i: List[Inversion]): List[ChInv] =
    (t.map(x => i map { y => ChInv(Triad(x), y) })).flatten

  /*
  def followPoss(c: Chord) /* extends PartialFunction[??]*/ : List[List[Chord]] = {
    ???
  }*/

}




