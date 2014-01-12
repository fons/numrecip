package com.github.fons.nr.ode.butcher.tableau

/**
 * Created with IntelliJ IDEA.
 * User: fons
 * Date: 11/10/13
 * Time: 8:59 AM
 * To change this template use File | Settings | File Templates.
 */
trait ButcherTableauT {

  val beta_list: List[List[Double]]
  val alfa_list: List[Double]
  val kappa_list: List[Double]
  val kerr_list: Option[List[Double]] //error list is optional; only fro embedded methods

  // reverse the entries in the butcher tableau since we use a stack as te main processing structure

  def beta(): List[List[Double]] = beta_list map (_ reverse)

  def alfa(): List[Double] = alfa_list

  def kappa(): List[Double] = kappa_list reverse

  def err(): Option[List[Double]] = {
    kerr_list match {
      case Some(elist) => Some((kappa_list, elist).zipped map (_ - _) reverse)
      case None => None
    }
  }

  protected def className[A](a: A)(implicit m: Manifest[A]) = m.toString

  def tableauName = "com.mhsw.com.github.fons.nr.ode.butcher.tableau.ButcherTableauT"

}
