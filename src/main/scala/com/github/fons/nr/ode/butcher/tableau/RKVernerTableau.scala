package com.github.fons.nr.ode.butcher.tableau

/**
 * Created with IntelliJ IDEA.
 * User: fons
 * Date: 11/18/13
 * Time: 6:50 PM
 * From J.C. Butcher Numerical Methods for Ordinary Diffrential Equations, 2nd edition, paragraph 335.
 *
 */
trait RKVernerTableau extends ButcherTableauT {
  override val alfa_list: List[Double] = List(0, 1.0 / 18.0, 1.0 / 6.0, 2.0 / 9.0, 2.0 / 3.0, 1.0, 8.0 / 9.0, 1.0)
  override val beta_list: List[List[Double]] =
    List(List(),
      List(1.0 / 18.0),
      List(-1.0 / 12.0, 1.0 / 4.0),
      List(-2.0 / 81.0, 4.0 / 27.0, 8.0 / 81.0),
      List(40.0 / 33.0, -4.0 / 11.0, -56.0 / 11.0, 54.0 / 11.0),
      List(-369.0 / 73.0, 72.0 / 73.0, 5380.0 / 219.0, -12285.0 / 584.0, 2695.0 / 1752.0),
      List(-8716.0 / 891.0, 656.0 / 297.0, 39520.0 / 891.0, -416.0 / 11.0, 52.0 / 27.0, 0.0),
      List(3015.0 / 256.0, -9.0 / 4.0, -4219.0 / 78.0, 5985.0 / 128.0, -539.0 / 384.0, 0, 693.0 / 3328.0)
    )
  override val kappa_list: List[Double] = List(3.0 / 80, 0.0, 4.0 / 25.0, 243.0 / 1120.0, 77.0 / 160.0, 73.0 / 700.0, 0.0, 0.0)
  override val kerr_list: Option[List[Double]] = Some(List(57.0 / 640.0, 0, -16.0 / 65.0, 1377.0 / 2240.0, 121.0 / 320.0, 0.0, 891.0 / 8320.0, 2.0 / 35.0))

  override def tableauName = className(this)
}
