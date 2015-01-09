package gen

import chord._
import tonalSystem.Tone._

object Chord {
  implicit def Chord2CConstr(c: Chord): CConstr = {
    c match {
      //better enforcement of composer constraints like that; makes more sense, too
      //      case Triad(I(_, None)) => ChInvPoss(c, Set(Fond, Inv1, Inv2))
      //      case Triad(x) => ChInvPoss(c, Set(Fond, Inv1))
      //      case Seventh(x) => ChInvPoss(c, Set(Fond, Inv1, Inv2, Inv3))
      case _ => ChInvPoss(c, Set(Fond))
    }
  }
}

trait CConstr
case object NoCons extends CConstr
case class ChInvPoss(c: Chord, i: Set[Inversion]) extends CConstr

trait HavePrev

case class ChInv(c: Chord, i: Inversion) extends HavePrev

class ChiEnd extends HavePrev with CConstr
case object EndReal extends ChiEnd //authentic cadence
case object EndMiddle extends ChiEnd // authentic cadence or deceptive cadence
case object EndHalf extends ChiEnd // half cadence
case object NoEnd extends ChiEnd //no particular cadence;
	//for linear harmonizer the default will be used (EndReal)
	//for constraints-based harmonizer, one of the possible chords will be used for the end

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