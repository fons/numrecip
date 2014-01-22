package com.github.fons.nr.examples

import com.github.fons.nr.interpolation._
import com.github.fons.nr.matrix.LUSolver
import scala.Predef._
import com.github.fons.nr.interpolation.Interpolator

/**
 * Created with IntelliJ IDEA.
 * User: fons
 * Date: 1/22/14
 * Time: 1:03 PM
 * To change this template use File | Settings | File Templates.
 */
case object CubicSplineExample3 {

    def S2(x:Double) = {
      val w = (x - 2.0)
      2.0 + w * (0.875 - 1.375 * w)
    }
    def run {
      val ll = Vector(0.0, 1.0, 2.0, 3.0)
      val vs = Vector(0.0, 0.5, 2.0, 1.5)
      val cs = new Interpolator(DataSet(ll, vs)) with CubicSpline with ParabolicallyTerminatedSpline with LUSolver
      println(ll)
      println(vs)
      println(cs)
      println("result  x= 2.1 : ")
      cs(2.1).map(_.map(_.map(println _)))
      println("exaxt :" , S2(2.1))
      println("======================================")
    }
}
