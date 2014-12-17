package gen

import chord._

object Chord {
  implicit def Chord2CConstr(c: Chord): CConstr = {
    c match {
      case Triad(x) => ChInv(c, Set(Fond, Inv1, Inv2))
      case Seventh(x) => ChInv(c, Set(Fond, Inv1, Inv2, Inv3))
      case _ => ChInv(c, Set(Fond, Inv1, Inv2)) //TODO : see for other what to put
    }
  }
}

//object Chord {
//  implicit def ctoint(c : Chord) : Int = 1
//}

trait CConstr
case object NoCons extends CConstr

class ChI

case class ChInv(c: Chord, i: Set[Inversion]) extends ChI with CConstr {
  def canIntersect(that: CConstr): Boolean = {
    that match {
      case NoCons => false
      case ChInv(c1, i1) => c1 == c && i.intersect(i1).nonEmpty
    }
  }
  def intersect(that: CConstr): ChInv = {
    that match {
      case NoCons => sys.error("no NoCons should be given to this function")
      case ChInv(c1, i1) => if (c1 != c) {
        sys.error("should use canIntersect before using intersect")
      } else {
        ChInv(c, i.intersect(i1))
      }
    }
  }
}

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