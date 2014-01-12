package com.github.fons.nr.matrix

import scala.Some

/**
 * Created with IntelliJ IDEA.
 * User: fons
 * Date: 12/13/13
 * Time: 3:58 PM
 * To change this template use File | Settings | File Templates.
 */
case object LUSolver extends LinearSystemsSolverT {
  override def apply(m: Matrix, c: Matrix): Option[Matrix] = {
    LUFactorization(m) match {
      case Some((l, u, p)) => ForwardSubstitution(l, (p * c)).flatMap(BackwardSubstitution(u, _))
      case _ => None
    }

  }

  override lazy val toString = productPrefix + " using " + LUFactorization
}
