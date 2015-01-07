package harm
import org.scalatest.FunSuite
import tonalSystem.Tone._
import gen.ToneOrdering

class OrderingTest extends FunSuite {

  test("compare") {
    val a = I(1, None)
    val b = I(2, None)

    assert(ToneOrdering.equiv(a, a))
    assert(ToneOrdering.lt(a, b))

    val c = I(1, Some(true))
    assert(ToneOrdering.lt(a, c))
    assert(ToneOrdering.gt(c, a))

    assert(ToneOrdering.equiv(O, O))
    assert(ToneOrdering.lt(O, a))
    assert(ToneOrdering.gt(a, O))

  }

}