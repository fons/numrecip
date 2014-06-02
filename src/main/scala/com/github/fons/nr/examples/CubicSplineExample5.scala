package com.github.fons.nr.examples

import com.github.fons.nr.interpolation._
import com.github.fons.nr.matrix.LUSolver
import scala.Predef._
import com.github.fons.nr.interpolation.Interpolator
import com.github.fons.nr.interpolation.SecondOrderDerivs

/**
 * Created with IntelliJ IDEA.
 * User: fons
 * Date: 1/22/14
 * Time: 5:19 PM
 * To change this template use File | Settings | File Templates.
 */
object CubicSplineExample5 {
  def S2(x: Double) = {
    val w = (x - 2.0)
    2.0 + w * (0.45 + w * (-2.25 + w * 1.30))
  }

  def run {
    val ll = Vector(0.0, 1.0, 2.0, 3.0)
    val vs = Vector(0.0, 0.5, 2.0, 1.5)
    val cs = new {
      val derivs = SecondOrderDerivs(-0.3, 3.3)
    } with Interpolator(DataSet(ll, vs)) with CubicSpline with CurvatureAdjustedSpline with LUSolver

    println(ll)
    println(vs)
    println(cs)
    println("result  x= 2.1 : ")
    cs(2.1).flatMap(_()).map(println _)
    println("exaxt :", S2(2.1))
    println("======================================")
  }
}
