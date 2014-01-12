package com.github.fons.nr.matrix

/**
 * Created with IntelliJ IDEA.
 * User: fons
 * Date: 1/8/14
 * Time: 6:38 PM
 * To change this template use File | Settings | File Templates.
 */

import scala.util.{Try, Success, Failure}
import scala.Some

object Factory {
  def apply(solver: Solver.Type): Option[LinearSystemsSolverT] = {
    solver match {
      case (Solver.GaussJordanFullPivot) => Some(PartialGaussJordanFullPivot)
      case (Solver.GaussJordanRowPivot) => Some(PartialGaussJordanRowPivot)
      case (Solver.IterativeGaussSeidel) => Some(GaussSeidelSolver())
      case (Solver.IterativeJacobi) => Some(JacobiSolver())
      case (Solver.LUSolver) => Some(LUSolver)
      case _ => None //Failure(new IllegalArgumentException("no ode factory method for solver type " + solver))
    }
  }

}
