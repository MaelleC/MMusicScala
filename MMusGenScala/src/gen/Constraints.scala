package gen

import cafesat.api.API._

object Constraints {
  def exactlyOne(cs: List[Formula]): List[Formula] = {
    //at least one variable is true
    val atLeastOne = List(or(cs: _*))

    //no more than one variable is true
    val noMoreThanOne1 = noMoreThanOne(cs)

    atLeastOne ++ noMoreThanOne1
  }

  def noMoreThanOne(cs: List[Formula]): List[Formula] = {
    val formulas = for (c1 <- 0 until (cs.length - 1); c2 <- (c1 + 1) until cs.length)
      yield !cs(c1) || !cs(c2)
    formulas.toList
  }

  def exactlyOnePair(cp: List[(Formula, Formula)], cs: List[Formula]): List[Formula] = {
    //link between a pair of variable and the variable that represents it
    val linkPair = (cp zip cs) flatMap { //((c1, c2), p)
      //p => c1 && c2 <=> (!p||c1) && (!p||c2)
      x =>
        (!x._2 || x._1._1) :: (!x._2 || x._1._2) ::
          //c1 && c2 => p <=> (p||!c1||!c2)
          or(x._2, !x._1._1, !x._1._2) :: Nil
    }

    //want one pair true exactly
    val exactlyOnePair0 = exactlyOne(cs)

    linkPair ++ exactlyOnePair0
  }
}