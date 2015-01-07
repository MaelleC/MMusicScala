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

  //-----------------------------------------------------------------------------------------
  //if you want to add chords :
  //add them in allChords list,
  // take care of them in possChInv (which is in getPossChords) and in prevPoss
  // also in file "Inversions", in Chord2CConstr
  //-----------------------------------------------------------------------------------------

  val allChords: List[Chord] = List(Triad(I), Triad(II), Triad(III),
    Triad(IV), Triad(V), Triad(VI), Triad(VII), Seventh(V))

  val nbChordNotes = 4; //for basic note placement this is better than 3

  def harmonize(endF: ChiEnd, useConstraintsSolver: Boolean = false, composerConstraints: List[(Int, List[CConstr])] = Nil): (MusicalSegment, ParallelSegment) = {
    val mel = getMel(melody)
    val minNote = (mel.notes).filter { x =>
      x.tone match {
        case O => false
        case _ => true
      }
    }.min(NoteOrdering)

    val melT = mel.notes map (_.tone)
    val compc: List[List[CConstr]] = {
      composerConstraints match {
        case Nil => Nil
        case _ => getConsList(composerConstraints, melT.length)
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

    val seed = Random.nextInt()
    val chosenChords = {
      if (!useConstraintsSolver) findChords(possibleChords, endF, seed)
      else findChordsC(possibleChords, endF, (compc.isEmpty || !compcForEnd)) match {
        case Some(possC) => possC
        case None => {
          println("could not solve with constraints, try with the linear harmonizer for possible diagnostic,\n but not all composer constraints will be satisfied.")
          findChords(possibleChords, endF, seed)
        }
      }
    }

    println("Remark : notes are counted from 0")
    println("Chosen chords (with their inversion and index in list) : ")
    println(chosenChords.zipWithIndex)

    //returns the chords notes without the melody
    val chosenTonesL = findAllTones(chosenChords, melT, nbChordNotes)

    val chosenNotes = tonesToNotes(chosenTonesL, mel.notes)

    val transposed = transpose(chosenNotes)

    val maxNote = transposed.last.max(NoteOrdering)

    if (NoteOrdering.lt(minNote, maxNote)) {
      printWarn("The melody is lower than the accompaniment at some point\n " +
        "(min melody : " + minNote + ", max acc. : " + maxNote + ")",
        "You could put it one or more octaves higher, it could sound better.")
    }

    (mel, createPar(transposed))

  }

  def getMel(melody: MusicalSegment): MusicalSegment = {
    val flatMel = melody.flatAll
    if (flatMel.parDepth != 0) {
      getOneVoice(flatMel)
    } else flatMel
  }

  def getConsList(ori: List[(Int, List[CConstr])], melLen: Int): List[List[CConstr]] = {
    def noConsL(i1: Int, i2: Int): List[List[CConstr]] = (List.range(i1, i2) map { x => List(NoCons) })
    def getConsList0(buf: List[List[CConstr]], o: List[(Int, List[CConstr])]): List[List[CConstr]] = {
      if (o.isEmpty) {
        if (buf.length != melLen) buf ::: noConsL(buf.length, melLen)
        else buf
      } else {
        if (o.head._1 < buf.length) {
          if (o.head._1 == buf.length - 1) {
            printWarn("Duplicate constraint in constraints list for index " + o.head._1,
              "Superfluous constraint is dropped")
          } else {
            printWarn("Wrong order in constraints list for index " + o.head._1,
              " Constraint is dropped")
          }
          getConsList0(buf, o.tail)
        } else {
          val valid = {
            if (o.head._2.contains(NoCons)) {
              if (o.head._2.length != 1) printWarn("For index " + o.head._1, "all other than NoCons are not taken into account")
              List(NoCons)
            } else {
              o.head._2 filter { x =>
                x match {
                  case ChInvPoss(c, i) => allChords.contains(c) && i.nonEmpty && !i.exists(y => y.first > c.tones.length)
                  case NoCons => false //should not happen
                }
              }
            }
          }
          if (valid.length != o.head._2.length && !o.head._2.contains(NoCons)) {
            printWarn("Index " + o.head._1 + " : exists invalid constraints (chords not supported, or empty inversion list, or incompatible inversion)", "only the valid ones are kept")
          }
          getConsList0(buf ::: noConsL(buf.length, o.head._1) ::: List(valid), o.tail)
        }
      }
    }

    val indices = ori map { x => x._1 }
    if (indices.max >= melLen) error("Indices should be less than " + melLen + " (which is the length of the melody).")
    else if (indices.min < 0) error("Indices should be greater or equal to zero.")
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

        //Inv2 is not stable and should be avoided in default case
        case Triad(_) => List(ChInv(c, Fond), ChInv(c, Inv1))
        case Seventh(_) => List(ChInv(c, Fond), ChInv(c, Inv1), ChInv(c, Inv2), ChInv(c, Inv3))

        case _ if (c.tones.size == 3) => List(ChInv(c, Fond), ChInv(c, Inv1), ChInv(c, Inv2))
        case _ if (c.tones.size == 4) => List(ChInv(c, Fond), ChInv(c, Inv1), ChInv(c, Inv2), ChInv(c, Inv3))
        case _ => List(ChInv(c, Fond))
      }
    }

    t match {
      case O => List(ChInv(EmptyChord, Fond))
      //TODO ? special case for III(_, None) in the filter clause to avoid it ?
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
  //compc info is included in poss : mode "harmony rules"
  def findChordsC(poss: List[List[ChInv]], endF: HavePrev, useEnd: Boolean): Option[List[ChInv]] = {
    //TODO : see if do differently with end than with other constraints
    val possE = {
      if (useEnd && endF != NoEnd && poss.nonEmpty) poss.take(poss.length - 1) ::: List(poss.last.intersect(prevPoss(endF)))
      else poss
    }

    val consVarsCh: List[List[(ChInv, Formula)]] = possE map { x => x map { y => (y, boolVar()) } }
    val onlyOneChInv = ((consVarsCh map { x => x map { y => y._2 } }) map { x => Constraints.exactlyOne(x) }).flatten

    def possPairs(c1: List[(ChInv, Formula)], c2: List[(ChInv, Formula)]): List[(Formula, Formula)] = {
      (c2 map { x => mergeP(c1, (prevPoss(x._1), x._2)) }).flatten
    }
    def mergeP(prev: List[(ChInv, Formula)], poss: (List[ChInv], Formula)): List[(Formula, Formula)] = {
      for (i <- poss._1; j <- prev if j._1 == i) yield (j._2, poss._2)
    }
    def noTwice2(c1: List[(ChInv, Formula)], c2: List[(ChInv, Formula)]): List[Formula] = {
      val c1Prob = c1.filterNot { x => noProb2(x._1) }
      val c2Prob = c2.filterNot { x => noProb2(x._1) }

      (for (i <- c1Prob; j <- c2Prob if j._1.c == i._1.c) yield Constraints.noMoreThanOne(List(i._2, j._2))).flatten
    }

    def noProb2(ci: ChInv): Boolean = {
      //chords that have no problem of being repeated at interval of 2
      ci match {
        case ChInv(Triad(I(_, _)), Fond) => true
        case ChInv(Triad(V(_, _)), Fond) => true
        case _ => false
      }
    }

    def noTwice3(c1: List[(ChInv, Formula)], c2: List[(ChInv, Formula)]): List[Formula] = {
      val c1Prob = c1.filterNot { x => noProb3(x._1) }
      val c2Prob = c2.filterNot { x => noProb3(x._1) }

      (for (i <- c1Prob; j <- c2Prob if j._1.c == i._1.c) yield Constraints.noMoreThanOne(List(i._2, j._2))).flatten
    }

    def noProb3(ci: ChInv): Boolean = {
      //chords that have no problem of being repeated at interval of 3
      ci match {
        case ChInv(Triad(I(_, _)), Fond) => true
        case ChInv(Triad(V(_, _)), Fond) => true
        case ChInv(Triad(IV(_, _)), Fond) => true
        case _ => false
      }
    }

    val consVarsNoEmptyChord = consVarsCh.filterNot({ x =>
      x.exists({ y =>
        y._1 match {
          case ChInv(EmptyChord, _) => true
          case _ => false
        }
      })
    })

    val pairsFormulas = {
      if (consVarsNoEmptyChord.nonEmpty) (consVarsNoEmptyChord zip consVarsNoEmptyChord.tail) map { x => possPairs(x._1, x._2) }
      else Nil
    }

    val pairVars = pairsFormulas map { x => x map { y => boolVar() } }
    val onlyOnePairChInv = ((pairsFormulas zip pairVars) map { x => Constraints.exactlyOnePair(x._1, x._2) }).flatten

    val allConstraintsSimple = onlyOneChInv ++ onlyOnePairChInv
    val solSimple = solveForSatisfiability(and(allConstraintsSimple: _*)) match {
      case None => None
      case Some(result) => Some(consVarsCh map { x => (x.filter { y => result(y._2) }).head._1 })
    }

    val int2PairsCons = {
      if (solSimple.isDefined && consVarsNoEmptyChord.nonEmpty && consVarsNoEmptyChord.tail.nonEmpty) {
        ((consVarsNoEmptyChord zip consVarsNoEmptyChord.tail.tail) map { x => noTwice2(x._1, x._2) }).flatten
      } else {
        Nil
      }
    }

    val int3PairsCons = {
      if (solSimple.isDefined && consVarsNoEmptyChord.nonEmpty && consVarsNoEmptyChord.tail.nonEmpty && consVarsNoEmptyChord.tail.tail.nonEmpty) {
        ((consVarsNoEmptyChord zip consVarsNoEmptyChord.tail.tail.tail) map { x => noTwice3(x._1, x._2) }).flatten
      } else {
        Nil
      }
    }

    val allConstraints = allConstraintsSimple ++ int2PairsCons ++ int3PairsCons
    val solMore = {
      if (solSimple.isDefined && int2PairsCons.nonEmpty) {
        println("Could solve with basic constraints, try with more.")
        solveForSatisfiability(and(allConstraints: _*)) match {
          case None => {
            println("Could not solve with more constraints.")
            None
          }
          case Some(result) => Some(consVarsCh map { x => (x.filter { y => result(y._2) }).head._1 })
        }
      } else {
        None
      }
    }

    if (solMore.isDefined) {
      println("Solved with all constraints")
      solMore
    } else solSimple

  }

  //find chords without formal constraints, linearly
  def findChords(poss: List[List[ChInv]], endF: HavePrev, seed: Int): List[ChInv] = {
    println("Seed : " + seed)
    val rand = new Random(seed)

    def findChord(next: HavePrev, possC: List[List[ChInv]], buf: List[ChInv], forceEnd: Boolean, nCh: Int): List[ChInv] = {
      if (possC.isEmpty) return buf //finished
      else if (possC.head.isEmpty) {
        //bizarre note
        if (forceEnd || buf.isEmpty) {
          //force a chord from prev of next
          val nextC = rand.shuffle(prevPoss(next)).head
          printWarnNote(nCh, "no chord compatible with the note, too bizarre note",
            "a random End-compatible chord is chosen.")
          findChord(nextC, possC.tail, nextC :: buf, false, nCh - 1)
        } else {
          //keep the previous chord
          val nextC = buf.head
          printWarnNote(nCh, "no chord compatible with the note, too bizarre note",
            "previous chord is kept.")
          findChord(nextC, possC.tail, nextC :: buf, false, nCh - 1)
        }
      } else if (possC.head.head.c == EmptyChord) {
        //silent : give silent chord, but continues harmony for next
        findChord(next, possC.tail, ChInv(EmptyChord, Fond) :: buf, forceEnd, nCh - 1)
      } else {
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
            findChord(nextC, possC.tail, nextC :: buf, false, nCh - 1)
          } else {
            if (forceEnd) {
              //force a chord from prev of next
              printWarnNote(nCh, "not compatible with the given End",
                "a random End-compatible chord is chosen.")
              val nextC = rand.shuffle(prevPoss(next)).head
              findChord(nextC, possC.tail, nextC :: buf, false, nCh - 1)
            } else {
              //take a possible chord, even if harmonically not ok
              printWarnNote(nCh, "not chord harmonically ok is compatible with the note",
                "a random chord in the compatible ones is chosen.")
              val nextC = rand.shuffle(possChI).head
              findChord(nextC, possC.tail, nextC :: buf, false, nCh - 1)
            }

          }

        } else {
          //normal case : random between ok chords
          val nextC = rand.shuffle(inter).head
          findChord(nextC, possC.tail, nextC :: buf, false, nCh - 1)
        }
      }

    }

    findChord(endF, poss.reverse, Nil, true, poss.length - 1)
  }

  /**
   * returns a sequential list of parallel list of notes, without the melody
   */
  def findAllTones(chords: List[ChInv], mel: List[Tone], nbNotes: Int): List[List[Tone]] = {

    def findTones(ch: ChInv /*, melN: Tone*/ ): List[Tone] = {
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
        case _ => (0 until nbNotes).toList map { y => ch.c(y + (ch.i.first % ch.c.tones.length)) }
      }
    }

    (chords zip mel) map { x =>
      {
        val re = findTones(x._1 /*, x._2*/ )
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

  // TODO ? : if V7+ -> I, I has to be Fond, can't be I6
  //TODO : relax some constraints (fi1 really necessary always ?), only for I probably is needed, but think more
  //TODO : put in prevPoss things for all chords of possibleChords !!!
  def prevPoss(ci: HavePrev): List[ChInv] = {
    ci match {
      case ChInv(Triad(I(_, None)), i) if fi1(i) => possInvToInv(ChInvPoss(Seventh(V), Set(Fond, Inv1, Inv2, Inv3))) ::: HarmFct(V)
      case ChInv(Triad(II(_, None)), i) if fi1(i) => HarmFct(I)
      case ChInv(Triad(III(_, None)), i) if fi1(i) => ??? //no need yet
      case ChInv(Triad(IV(_, None)), i) if fi1(i) => HarmFct(I)
      case ChInv(Triad(V(_, None)), i) if fi1(i) => HarmFct(I) ::: HarmFct(IV)
      case ChInv(Triad(VI(_, None)), i) if fi1(i) => getCiL(List(V), List(Fond, Inv1))
      case ChInv(Triad(VII(_, None)), i) if fi1(i) => HarmFct(I) ::: HarmFct(IV)
      case ChInv(Triad(I(_, None)), Inv2) => HarmFct(IV)
      case ChInv(Seventh(V(o, None)), i) => prevPoss(ChInv(Triad(V(o, None)), Fond))
      case EndReal => List(ChInv(Triad(I), Fond))
      case EndMiddle => ChInv(Triad(I), Fond) :: getCiL(List(VI), List(Fond, Inv1))
      case EndHalf => List(ChInv(Triad(V), Fond))
      case NoEnd => error("no NoEnd should go into prevPoss")
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

  def printWarnNote(nb: Int, str1: String, str2: String) =
    printWarn("note " + nb + " : " + str1, str2)
  def printWarn(str1: String, str2: String) =
    println("warn : " + str1 + ".\n  " + str2)

}




