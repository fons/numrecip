package com.github.fons.nr.ode.butcher.tableau

/**
 * Created with IntelliJ IDEA.
 * User: fons
 * Date: 11/17/13
 * Time: 6:45 PM
 * From J.C. Butcher Numerical Methods for Ordinary Diffrential Equations, 2nd edition, paragraph 232
 * same as com.mhsw.com.github.fons.nr.ode.butcher.tableau.ExplicitMidPointTableau
 */
trait RK22Tableau extends ButcherTableauT {
  override val alfa_list: List[Double] = List(0, 1.0 / 2.0)
  override val beta_list: List[List[Double]] = List(List(), List(1.0 / 2.0))
  override val kappa_list: List[Double] = List(0.0, 1.0)
  override val kerr_list: Option[List[Double]] = None

  override def tableauName = className(this) + "(no error list)"
}
