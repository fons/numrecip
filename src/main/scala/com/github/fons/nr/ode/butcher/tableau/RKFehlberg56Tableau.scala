package com.github.fons.nr.ode.butcher.tableau

/**
 * Created with IntelliJ IDEA.
 * User: fons
 * Date: 11/19/13
 * Time: 6:28 PM
 *
 * From J.C. Butcher Numerical Methods for Ordinary Diffrential Equations, 2nd edition, paragraph 335.
 *
 *
 */
trait RKFehlberg56Tableau extends ButcherTableauT {
  override val alfa_list: List[Double] = List(0, 1.0 / 6.0, 4.0 / 15.0, 2.0 / 3.0, 4.0 / 5.0, 1.0, 0.0, 1.0)
  override val beta_list: List[List[Double]] =
    List(List(),
      List(1.0 / 6.0),
      List(4.0 / 75.0, 16.0 / 75.0),
      List(5.0 / 6.0, -8.0 / 3.0, 5.0 / 2.0),
      List(-8.0 / 5.0, 144.0 / 25.0, -4.0, 16.0 / 25.0),
      List(361.0 / 320.0, -18.0 / 5.0, 407.0 / 128.0, -11.0 / 80.0, 55.0 / 128.0),
      List(-11.0 / 640.0, 0.0, 11.0 / 256.0, -11.0 / 160.0, 11.0 / 256.0, 0.0),

      List(93.0 / 640.0, -18.0 / 5.0, 803.0 / 256.0, -11.0 / 160.0, 99.0 / 256.0, 0.0, 1.0)

    )
  override val kappa_list: List[Double] = List(31.0 / 384.0, 0.0, 1125.0 / 2816.0, 9.0 / 32.0, 125.0 / 768.0, 5.0 / 66.0, 0.0, 0.0)
  override val kerr_list: Option[List[Double]] = Some(List(7.0 / 1408.0, 0.0, 1125.0 / 2816.0, 9.0 / 32.0, 125.0 / 768.0, 0.0, 5.0 / 66.0, 5.0 / 66.0))

  override def tableauName = className(this)
}
