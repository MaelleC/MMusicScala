package gen

import chord._

object Chord {
  implicit def Chord2CConstr(c: Chord): CConstr = {
    c match {
      case Triad(x) => ChInvPoss(c, Set(Fond, Inv1, Inv2))
      case Seventh(x) => ChInvPoss(c, Set(Fond, Inv1, Inv2, Inv3))
      case _ => ChInvPoss(c, Set(Fond, Inv1, Inv2)) //TODO : see for other what to put
    }
  }
}

trait CConstr
case object NoCons extends CConstr
case class ChInvPoss(c: Chord, i: Set[Inversion]) extends CConstr

trait HavePrev

case class ChInv(c: Chord, i: Inversion) extends HavePrev

class ChiEnd extends HavePrev with CConstr
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