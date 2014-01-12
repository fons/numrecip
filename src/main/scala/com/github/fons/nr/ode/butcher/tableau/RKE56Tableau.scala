package com.github.fons.nr.ode.butcher.tableau

/**
 * Created with IntelliJ IDEA.
 * User: fons
 * Date: 11/18/13
 * Time: 7:11 PM
 *
 * From J.C. Butcher Numerical Methods for Ordinary Diffrential Equations, 2nd edition, paragraph 333.
 * Order 5 embedded with order 6 requires 8 stages.
 *
 * This is set up so that the 6th order estimate is returned. Be aware that for the 5th order estimate
 * the last row in the beta list needs to be dropped.
 *
 */
trait RKE56Tableau extends ButcherTableauT {
  override val alfa_list: List[Double] = List(0, 1.0 / 9.0, 1.0 / 9.0, 1.0 / 6.0, 1.0 / 3.0, 1.0 / 2.0, 3.0 / 4.0, 17.0 / 19.0, 1.0)
  override val beta_list: List[List[Double]] =
    List(List(),
      List(1.0 / 9.0),
      List(1.0 / 18.0, 1.0 / 18.0),
      List(1.0 / 24.0, 0.0, 1.0 / 8.0),
      List(1.0 / 6.0, 0.0, -1.0 / 2.0, 2.0 / 3.0),
      List(15.0 / 8.0, 0.0, -63.0 / 8.0, 7, -1.0 / 2.0),
      List(-93.0 / 22.0, 0.0, 24921.0 / 1408.0, -10059.0 / 704.0, 735.0 / 1408.0, 735.0 / 704.0),
      List(86547313055.0 / 10295610642.0, 0.0, -96707067.0 / 2867062.0, 15526951598.0 / 571978869.0, 27949088.0 / 81711267.0, -452648800.0 / 245133801.0, 270189568.0 / 467982711.0),
      List(98.0 / 765.0, 0.0, 0.0, -9.0 / 83.0, 1071.0 / 1600.0, -11.0 / 75.0, 64.0 / 225.0, 390963.0 / 2257600)
    )
  override val kappa_list: List[Double] = List(188.0 / 3315.0, 0.0, 0.0, 1593.0 / 7553.0, 2943.0 / 20800.0, 197.0 / 975.0, 576.0 / 2275.0, 2476099.0 / 29348800.0, 2.0 / 39.0)

  override val kerr_list: Option[List[Double]] = Some(List(98.0 / 765.0, 0.0, 0.0, -9.0 / 83.0, 1071.0 / 1600.0, -11.0 / 75.0, 64.0 / 225.0, 390963.0 / 2257600.0, 0.0))

  override def tableauName = className(this)
}
