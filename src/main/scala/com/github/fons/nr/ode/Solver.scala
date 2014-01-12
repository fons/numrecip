package com.github.fons.nr.ode

/**
 * Created with IntelliJ IDEA.
 * User: fons
 * Date: 11/22/13
 * Time: 8:34 PM
 * To change this template use File | Settings | File Templates.
 */
object Solver extends Enumeration {

  type Type = Value
  val RK4 = Value(0, "ExplicitRungeKutta4")
  val EM = Value(1, "ExplicitMidPoint")
  val EU = Value(2, "Euler")
  val RK5 = Value(3, "RungeKuttaWithRk5Tableau")
  val RKF56 = Value(4, "RKEmbeddedFehlberg56")
  val RKF78 = Value(5, "RKEmbeddedFehlberg78")
  val RKDP1 = Value(6, "RKEmbeddedDormandPrince54")
  val RKDP2 = Value(7, "RKEmbeddedDormandPrince")
  val RKV = Value(8, "RKEmbeddedVerner")
  val RKCK = Value(9, "RKEmbeddedCashKarp")
  val RK42 = Value(10, "RungeKutta42")
  val BS23 = Value(12, "RKBS23fromMatLab")
  val RKE56 = Value(13, "RKEmbedded56")
  val RKE23 = Value(14, "RKEmbedded23")
}
