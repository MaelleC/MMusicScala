package harm
import org.scalatest.FunSuite
import chord._
import tonalSystem.Tone._

class ChordEqualityTest extends FunSuite {

  test("chordEqualityModuloOctave") {
    assert(Triad(I(0, None)) == Triad(I(1, None)))
    assert(Triad(I(0, Some(true))) != Triad(I(1, None)))
  }
}