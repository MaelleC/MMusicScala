package gen

import chord._

//TODO : does not work
//  val b : List[(Int, List[CConstr])] = (1, List(ChInv(Triad(I), List(Fond)))) :: Nil
//  val c : List[(Int, List[CConstr])] = (1, List(Triad(I))) :: Nil
object ConsImplicits {
  implicit def Chord2ChInv(c: Chord): CConstr = {
    c match {
      case Triad(x) => ChInv(c, List(Fond, Inv1, Inv2))
      case Seventh(x) => ChInv(c, List(Fond, Inv1, Inv2, Inv3))
      case _ => ChInv(c, List(Fond, Inv1, Inv2)) //TODO : see for other what to put
    }

  }
}

trait CConstr
case object NoCons extends CConstr

class ChI
case class ChInv(c: Chord, i: List[Inversion]) extends ChI with CConstr
class ChiEnd extends ChI with CConstr
case object EndReal extends ChiEnd //want real complete cadence
case object EndMiddle extends ChiEnd // deceptive cadence can be ok
case object EndHalf extends ChiEnd
case object NoEnd extends ChiEnd

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