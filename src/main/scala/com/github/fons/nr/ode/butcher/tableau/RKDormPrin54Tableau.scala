package com.github.fons.nr.ode.butcher.tableau

/**
 * Created with IntelliJ IDEA.
 * User: fons
 * Date: 11/18/13
 * Time: 6:01 AM
 *
 * From J.C. Butcher Numerical Methods for Ordinary Diffrential Equations, 2nd edition, paragraph 336.
 * Dormand and Prince embedded RK method.
 * Highes order is returned
 *
 */
trait RKDormPrin54Tableau extends ButcherTableauT {
  override val alfa_list: List[Double] = List(0, 1.0 / 5.0, 3.0 / 10.0, 4.0 / 5.0, 8.0 / 9.0, 1.0, 1.0)
  override val beta_list: List[List[Double]] =
    List(List(),
      List(1.0 / 5.0),
      List(3.0 / 40.0, 9.0 / 40.0),
      List(44.0 / 45.0, -56.0 / 15.0, 32.0 / 9.0),
      List(19372.0 / 6561.0, -25360.0 / 2187.0, 64448.0 / 6561.0, -212.0 / 729.0),
      List(9017.0 / 3168.0, -355.0 / 33.0, 46732.0 / 5247.0, 49.0 / 176.0, -5103.0 / 18656.0),
      List(35.0 / 384.0, 0.0, 500.0 / 1113.0, 125.0 / 192.0, -2187.0 / 6784.0, 11.0 / 84.0))
  override val kappa_list: List[Double] = List(35.0 / 384.0, 0.0, 500.0 / 1113.0, 125.0 / 192.0, -2187.0 / 6784.0, 11.0 / 84.0, 0.0)
  override val kerr_list: Option[List[Double]] = Some(List(5179.0 / 57600.0, 0, 7571.0 / 16695.0, 393.0 / 640.0, -92097.0 / 339200.0, 187.0 / 2100.0, 1.0 / 40.0))

  override def tableauName = className(this)
}
