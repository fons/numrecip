package com.github.fons.nr.ode.butcher.tableau

/**
 * Created with IntelliJ IDEA.
 * User: fons
 * Date: 11/18/13
 * Time: 6:39 PM
 * From J.C. Butcher Numerical Methods for Ordinary Diffrential Equations, 2nd edition, paragraph 336.
 * Dormand and Prince embedded RK method.
 * Bascially table 336B hence the name
 * Highes order is returned
 */
trait RKDormPrin336BTableau extends ButcherTableauT {
  override val alfa_list: List[Double] = List(0, 2.0 / 9.0, 1.0 / 3.0, 5.0 / 9.0, 2.0 / 3.0, 1.0, 1.0)
  override val beta_list: List[List[Double]] =
    List(List(),
      List(2.0 / 9.0),
      List(1.0 / 12.0, 1.0 / 4.0),
      List(55.0 / 324.0, -25.0 / 108.0, 50.0 / 81.0),
      List(83.0 / 330.0, -13.0 / 22.0, 61.0 / 66.0, 9.0 / 110.0),
      List(-19.0 / 28.0, 9.0 / 4.0, 1.0 / 7.0, -27.0 / 7.0, 22.0 / 7.0),
      List(19.0 / 200.0, 0.0, 3.0 / 5.0, -243.0 / 400.0, 33.0 / 40.0, 7.0 / 80.0))
  override val kappa_list: List[Double] = List(19.0 / 200.0, 0.0, 3.0 / 5.0, -243.0 / 400.0, 33.0 / 40.0, 7.0 / 80.0, 0.0)
  override val kerr_list: Option[List[Double]] = Some(List(431.0 / 5000.0, 0, 333.0 / 500.0, -7857.0 / 10000.0, 957.0 / 1000.0, 193.0 / 2000.0, -1.0 / 50.0))

  override def tableauName = className(this)
}
