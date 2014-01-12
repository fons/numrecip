package com.github.fons.nr.ode.butcher.tableau

/**
 * Created with IntelliJ IDEA.
 * User: fons
 * Date: 11/17/13
 * Time: 7:06 PM
 *
 * From J.C. Butcher Numerical Methods for Ordinary Diffrential Equations, 2nd edition, paragraph 236.
 *
 */
trait RK5Tableau extends ButcherTableauT {

  override val alfa_list: List[Double] = List(0, 1.0 / 4.0, 1.0 / 4.0, 1.0 / 2.0, 3.0 / 4.0, 1.0)
  override val beta_list: List[List[Double]] = List(List(), List(1.0 / 4.0),
    List(1.0 / 8.0, 1.0 / 8.0),
    List(0.0, 0.0, 1.0 / 2.0),
    List(3.0 / 16.0, -3.0 / 8.0, 3.0 / 8.0, 9.0 / 16.0),
    List(-3.0 / 7.0, 8.0 / 7.0, 6.0 / 7.0, -12.0 / 7.0, 8.0 / 7.0))
  override val kappa_list: List[Double] = List(7.0 / 90.0, 0.0, 32.0 / 90.0, 12.0 / 90.0, 32.0 / 90.0, 7.0 / 90.0)
  override val kerr_list: Option[List[Double]] = None

  override def tableauName = className(this) + "(no error list)"
}
