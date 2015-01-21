package gen

import segmentSystem._
import rythmics._

object SelectNotes {
  def firstAfterDuration(s: MusicalSegment, d: BPM): MusicalSegment = {
    val a = HarmonyGen(s)
    val mel = a.getMel(s)
    val melN = mel.notes

    def getNotes(orig: List[Note], buf: List[Note], c: Double): List[Note] = {
      if (c > d.computed)
        getNotes(orig, Note(buf.head.tone, BPM(d.computed + buf.head.duration.computed)) :: buf.tail, c - d.computed)
      //Note(buf.head.tone, d) :: buf
      else if (orig.isEmpty) buf
      else if (c + orig.head.duration.computed > d.computed) {
        val comp = {
          if (orig.tail.isEmpty) BPM(c + orig.head.duration.computed - d.computed)
          else d
        }
        getNotes(orig.tail, Note(orig.head.tone, comp) :: buf, c + orig.head.duration.computed - d.computed)
      } else {
        if (orig.tail.isEmpty) {
          Note(buf.head.tone, BPM(c + orig.head.duration.computed)) :: buf.tail
        } else getNotes(orig.tail, buf, c + orig.head.duration.computed)

      }
    }

    if (melN.nonEmpty) {

      if (melN.tail.isEmpty) toSequ(melN)
      else toSequ(getNotes(melN.tail, List(Note(melN.head.tone, d)), melN.head.duration.computed).reverse)
    } else EmptySeq

  }

  //From HarmonyGen : should group both somewhere
  def toSequ(voice: List[Note]): SequentialSegment = {
    voice.foldLeft[SequentialSegment](EmptySeq)((s, n) => s + n)
  }

  def totalDuration(n: List[Note]): Double = {
    def count(buf: Double, orig: List[Note]): Double = {
      if (orig.isEmpty) buf
      else count(buf + orig.head.duration.computed, orig.tail)
    }
    count(0, n)
  }

}