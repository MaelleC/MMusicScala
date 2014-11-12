package gen

import tonalSystem.Tone
import tonalSystem.Tone._
import segmentSystem._

//TODO : test and then put in Note / Tone after asking Valerian
object NoteOrdering extends Ordering[Note] {
  def compare(a: Note, b: Note) = {
    ToneOrdering.compare(a.tone, b.tone)
  }
}
object ToneOrdering extends Ordering[Tone] {
  def compare(a: Tone, b: Tone) = {
    if (a.stepsTo(b) < 0) 1
    else if (a.stepsTo(b) > 0) -1
    else if (a.alter == b.alter) 0
    else if (a.alter.isDefined) {
      if (a.alter.get) 1 else -1
    } else {
      if (b.alter.get) -1 else 1
    }
  }
}