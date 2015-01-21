package harm

import org.scalatest.FunSuite
import rythmics._
import utils.MelodyWriter
import segmentSystem._
import tonalSystem.Tone._
import gen.SelectNotes
import segmentSystem._

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

    assert(partHarm == res)
    assert(SelectNotes.totalDuration(partHarm.notes) == SelectNotes.totalDuration(res.notes))

  }

  test("end littler little notes") {
    val part = {
      IV / (4) + VI / (4) + I(1) / (0.5) +
        III / (4) + II / (4) +
        I / (4) + I(1) / (4) + II(1) / (4) + I(1) / (4) +
        VII / (2) + V / (4) + VI / (4) +
        VII / (4) + V / (4) + II / (2) +
        VI / (2) + I / (4) + VII / (4) +
        V / (2) + VII / (4) + V / (4) +
        IV(0, Some(true)) / (4) + II / (4)
    }

    val partHarm = {
      IV + I(1) / (0.5) + I + VII + VII + VI + V + IV(0, Some(true)) / (2)
    }

    val res = SelectNotes.firstAfterDuration(part, E)

    assert(partHarm == res)
    assert(SelectNotes.totalDuration(partHarm.notes) == SelectNotes.totalDuration(res.notes))

  }

  test("end littler big note") {
    val part = {
      IV / (4) + VI / (4) + I(1) / (0.5) +
        III / (4) + II / (4) +
        I / (4) + I(1) / (4) + II(1) / (4) + I(1) / (4) +
        VII / (2) + V / (4) + VI / (4) +
        VII / (4) + V / (4) + II / (2) +
        VI / (2) + I / (4) + VII / (4) +
        V / (2) + VII
    }

    val partHarm = {
      IV + I(1) / (0.5) + I + VII + VII + VI + V + VII / (2)
    }

    val res = SelectNotes.firstAfterDuration(part, E)

    assert(partHarm == res)
    assert(SelectNotes.totalDuration(partHarm.notes) == SelectNotes.totalDuration(res.notes))

  }

  test("one note") {
    val part: Note = {
      IV
    }

    val partHarm: Note = {
      IV
    }

    val res = SelectNotes.firstAfterDuration(part, E)

    val part1: Note = {
      IV / (2)
    }

    val partHarm1: Note = {
      IV / (2)
    }

    val res1 = SelectNotes.firstAfterDuration(part1, E)

    val part2: Note = {
      IV / (0.5)
    }

    val partHarm2: Note = {
      IV / (0.5)
    }

    val res2 = SelectNotes.firstAfterDuration(part2, E)

    val part3: Note = {
      IV / (2 / 3.0)
    }

    val partHarm3: Note = {
      IV / (2 / 3.0)
    }

    val res3 = SelectNotes.firstAfterDuration(part3, E)

    assert(partHarm.notes == res.notes)
    assert(SelectNotes.totalDuration(partHarm.notes) == SelectNotes.totalDuration(res.notes))

    assert(partHarm1.notes == res1.notes)
    assert(SelectNotes.totalDuration(partHarm1.notes) == SelectNotes.totalDuration(res1.notes))

    assert(partHarm2.notes == res2.notes)
    assert(SelectNotes.totalDuration(partHarm2.notes) == SelectNotes.totalDuration(res2.notes))

    assert(partHarm3.notes == res3.notes)
    assert(SelectNotes.totalDuration(partHarm3.notes) == SelectNotes.totalDuration(res3.notes))

  }

}