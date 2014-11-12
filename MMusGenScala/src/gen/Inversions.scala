package gen

import chord._

class ChI
case class ChInv(c: Chord, i: List[Inversion]) extends ChI
class ChiEnd extends ChI
case object EndReal extends ChiEnd //want real complete cadencs
case object EndMiddle extends ChiEnd // deceptive cadence can be ok
case object EndHalf extends ChiEnd

//TODO : perhaps represent differently ?
trait Inversion {
  val first: Int
}

case object Fond extends Inversion {
  val first = 0
}
case object Inv1 extends Inversion {
  val first = 1
}
case object Inv2 extends Inversion {
  val first = 2
}
case object Inv3 extends Inversion {
  val first = 3
}