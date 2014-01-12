package com.github.fons.nr.ode.butcher.tableau

/**
 * Created with IntelliJ IDEA.
 * User: fons
 * Date: 11/17/13
 * Time: 7:00 PM
 *
 * From J.C. Butcher Numerical Methods for Ordinary Diffrential Equations, 2nd edition, paragraph 235
 * Same as com.mhsw.com.github.fons.nr.ode.ExplicitRungeKutta4 which is optimized..
 * Basically an embedded 2nd method wit embedded thrd order (which corresponds to the classical RK method)
 * I'm returning the higher order estimate
 */
trait RKE23Tableau extends ButcherTableauT {
  override val alfa_list: List[Double] = List(0, 1.0 / 2.0, 1.0 / 2.0, 1.0)
  override val beta_list: List[List[Double]] = List(List(), List(1.0 / 2.0), List(0.0, 1.0 / 2.0), List(0.0, 0.0, 1.0))
  override val kappa_list: List[Double] = List(1.0 / 6.0, 1.0 / 3.0, 1.0 / 3.0, 1.0 / 6.0)
  override val kerr_list: Option[List[Double]] = Some(List(0.0, 0.0, 1.0, 0.0))

  // lowest order
  override def tableauName = className(this)
}
