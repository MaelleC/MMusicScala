package harm

import org.scalatest.FunSuite
import rythmics._
import utils.MelodyWriter
import segmentSystem._
import tonalSystem.Tone._
import gen.SelectNotes

class SelectTest extends FunSuite with MelodyWriter {
  implicit val noteDuration = E

  test("base") {
    val part = {
      IV / (4) + VI / (4) + I(1) / (4) + VI / (4) +
        V / (2) + III / (4) + II / (4) +
        I / (4) + I(1) / (4) + II(1) / (4) + I(1) / (4) +
        VII / (2) + V / (4) + VI / (4) +
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

    val partHarm = {
      IV + V + I + VII + VII + VI + V + IV(0, Some(true)) + V + I(1) + II(1) + II(1) + V + VI + IV + V + III(1) + I(1)
    }

    val res = SelectNotes.firstAfterDuration(part, E)

    assert(partHarm == res)
    assert(SelectNotes.totalDuration(partHarm.notes) == SelectNotes.totalDuration(res.notes))

  }

  test("note long duration") {
    val part = {
      IV / (4) + VI / (4) + I(1) / (0.5) +
        III / (4) + II / (4) +
        I / (4) + I(1) / (4) + II(1) / (4) + I(1) / (4) +
        VII / (2) + V / (4) + VI / (4) +
        VII / (4) + V / (4) + II / (2) +
        VI / (2) + I / (4) + VII / (4) +
        V / (2) + VII / (4) + V / (4) +
        IV(0, Some(true)) / (4) + II / (4) + III / (4) + IV(0, Some(true)) / (4)
    }

    val partHarm = {
      IV + I(1) / (0.5) + I + VII + VII + VI + V + IV(0, Some(true))
    }

    val res = SelectNotes.firstAfterDuration(part, E)

    println(partHarm)
    println(res)

    assert(partHarm == res)
    assert(SelectNotes.totalDuration(partHarm.notes) == SelectNotes.totalDuration(res.notes))

  }

}