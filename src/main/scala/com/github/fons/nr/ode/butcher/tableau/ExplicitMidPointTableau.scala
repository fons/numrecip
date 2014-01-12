package com.github.fons.nr.ode.butcher.tableau

/**
 * Created with IntelliJ IDEA.
 * User: fons
 * Date: 11/17/13
 * Time: 6:04 PM
 *
 * Butcher Tableau for the explicit mid point method
 * Greg von Winckel; Numerical Methods for Diffrential Equations;
 * Lecture Notes, Winter Semester, Institute for Mathematics and Scientific Computing
 *
 */
trait ExplicitMidPointTableau extends ButcherTableauT {
  override val alfa_list: List[Double] = List(0, 1.0 / 2.0)
  override val beta_list: List[List[Double]] = List(List(0), List(1.0 / 2.0, 0))
  override val kappa_list: List[Double] = List(1)
  override val kerr_list: Option[List[Double]] = Some(List(1))

  override def tableauName = className(this) + "(no error list)"
}
