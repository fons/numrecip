/*
 * Copyright (c) 2014.
 *
 * This file CubicSplineExample4.scala is part of numrecip (numrecip)
 *
 *     numrecip / CubicSplineExample4.scala is free software: you can redistribute it and/or modify
 *     it under the terms of the GNU General Public License as published by
 *     the Free Software Foundation, either version 3 of the License, or
 *     (at your option) any later version.
 *
 *     numrecip / CubicSplineExample4.scala is distributed in the hope that it will be useful,
 *     but WITHOUT ANY WARRANTY; without even the implied warranty of
 *     MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 *     GNU General Public License for more details.
 *
 *     You should have received a copy of the GNU General Public License
 *     along with numrecip.  If not, see <http://www.gnu.org/licenses/>.
 */

package com.github.fons.nr.examples

import com.github.fons.nr.interpolation._
import com.github.fons.nr.matrix.LUSolver
import scala.Predef._
import com.github.fons.nr.interpolation.Interpolator

/**
 * Created with IntelliJ IDEA.
 * User: fons
 * Date: 1/22/14
 * Time: 1:56 PM
 * To change this template use File | Settings | File Templates.
 */

object CubicSplineExample4 {
  def S2(x:Double) = {
    val w = (x - 2.0)
    2.0 + w * (0.68 + w * (-1.86 + w * 0.68 ))
  }
  def run {
    val ll = Vector(0.0, 1.0, 2.0, 3.0)
    val vs = Vector(0.0, 0.5, 2.0, 1.5)
    val cs = new { val derivs = FirstOrderDerivs(0.2, -1.0)} with Interpolator(DataSet(ll, vs)) with CubicSpline with ClampedSpline with LUSolver

    println(ll)
    println(vs)
    println(cs)
    println("result  x= 2.1 : ")
    cs(2.1).flatMap(_()).map(println _)
    println("exaxt :" , S2(2.1))
    println("======================================")
  }
}
