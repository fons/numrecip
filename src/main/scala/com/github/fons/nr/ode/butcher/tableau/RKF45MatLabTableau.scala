package com.github.fons.nr.ode.butcher.tableau

/**
 * Created with IntelliJ IDEA.
 * User: fons
 * Date: 10/29/13
 * Time: 6:47 PM
 *
 * from Numerical Methods using MATLAB 4th edition
 * John H. Mathews; Kurtis D. Fink, p497
 *
 *
 */
trait RKF45MatLabTableau extends ButcherTableauT {

  override val alfa_list: List[Double] = List(0, 1.0 / 4.0, 3.0 / 8.0, 12.0 / 13.0, 1.0, 1.0 / 2.0)

  override val beta_list: List[List[Double]] = List(List(),
    List(1.0 / 4.0),
    List(3.0 / 32.0, 9.0 / 32.0),
    List(1932.0 / 2197.0, -7200.0 / 2197.0, 7296.0 / 2197.0),
    List(439.0 / 216.0, -8.0, 3680.0 / 513.0, -845.0 / 4104.0),
    List(-8.0 / 27.0, 2.0, -3544.0 / 2565.0, 1859.0 / 4104.0, -11.0 / 40.0))

  override val kappa_list: List[Double] = List(16.0 / 135.0, 0, 6656.0 / 12825.0, 28561.0 / 56430.0, -9.0 / 50.0, 2.0 / 55.0)
  override val kerr_list: Option[List[Double]] = Some(List(25.0 / 216.0, 0, 1408.0 / 2565.0, 2197.0 / 4101.0, -1.0 / 5.0, 0.0))

  override def tableauName = className(this)
}
