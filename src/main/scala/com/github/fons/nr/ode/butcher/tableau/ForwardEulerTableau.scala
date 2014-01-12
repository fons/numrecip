package com.github.fons.nr.ode.butcher.tableau

/**
 * Created with IntelliJ IDEA.
 * User: fons
 * Date: 11/17/13
 * Time: 5:31 PM
 * To change this template use File | Settings | File Templates.
 */
trait ForwardEulerTableau extends ButcherTableauT {
  override val alfa_list: List[Double] = List(0)
  override val beta_list: List[List[Double]] = List(List())
  override val kappa_list: List[Double] = List(1)
  override val kerr_list: Option[List[Double]] = Some(List(1))

}
