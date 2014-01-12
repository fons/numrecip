package com.github.fons.nr.ode.butcher.tableau

/**
 * Created with IntelliJ IDEA.
 * User: fons
 * Date: 10/29/13
 * Time: 6:41 PM
 *
 * Cash-Karp Parameters for Embedded Rung-Kutta Method
 * from Numerical Recipes in C 2nd edition
 * William H. Press et.al.
 *
 */
trait CashKarpTableau extends ButcherTableauT {

  override val alfa_list: List[Double] = List(0, 1.0 / 5.0, 3.0 / 10.0, 3.0 / 5.0, 1.0, 7.0 / 8.0)
  override val beta_list: List[List[Double]] = List(List(),
    List(1.0 / 5.0),
    List(3.0 / 40.0, 9.0 / 40.0),
    List(3.0 / 10.0, -9.0 / 10.0, 6.0 / 5.0),
    List(-11.0 / 54.0, 5.0 / 2.0, -70.0 / 27.0, 35.0 / 27.0),
    List(1631.0 / 55296.0, 175.0 / 512.0, 575.0 / 13824.0, 44275.0 / 110592.0, 253.0 / 4096.0))
  override val kappa_list: List[Double] = List(37.0 / 378.0, 0, 250.0 / 621.0, 125.0 / 594.0, 0, 512.0 / 1771.0)
  override val kerr_list: Option[List[Double]] = Some(List(2825.0 / 27648.0, 0.0, 18575.0 / 48384.0, 13525.0 / 55296.0, 277.0 / 14336.0, 1.0 / 4.0))

  //private def className[A](a: A)(implicit m: Manifest[A]) = m.toString
  override def tableauName = className(this)
}
