/*
 * Copyright (c) 2014.
 *
 * This file CubicSplineExample1.scala is part of numrecip (numrecip)
 *
 *     numrecip / CubicSplineExample1.scala is free software: you can redistribute it and/or modify
 *     it under the terms of the GNU General Public License as published by
 *     the Free Software Foundation, either version 3 of the License, or
 *     (at your option) any later version.
 *
 *     numrecip / CubicSplineExample1.scala is distributed in the hope that it will be useful,
 *     but WITHOUT ANY WARRANTY; without even the implied warranty of
 *     MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 *     GNU General Public License for more details.
 *
 *     You should have received a copy of the GNU General Public License
 *     along with numrecip.  If not, see <http://www.gnu.org/licenses/>.
 */

package com.github.fons.nr.examples

import com.github.fons.nr.matrix.{LUSolver, PartialGaussJordanRowPivot}
import com.github.fons.nr.interpolation._

/**
 * Created with IntelliJ IDEA.
 * User: fons
 * Date: 1/19/14
 * Time: 3:02 PM
 * To change this template use File | Settings | File Templates.
 */

case object CubicSplineExample1 {
  def S2(x:Double) = {
    val w = (x - 2.0)
    2.0 + w*(0.7 + w*(0.6*w - 1.8))
  }
  def run {
    val ll = Vector(0.0, 1.0, 2.0, 3.0)
    val vs = Vector(0.0, 0.5, 2.0, 1.5)
    val cs = new Interpolator(DataSet(ll, vs)) with CubicSpline with NormalSpline with LUSolver
    println(ll)
    println(vs)
    println(cs)
    println("result  x= 2.1 : ")
    cs(2.1).map(_.map(_.map(println _)))
    println("exaxt :" , S2(2.1))
    println("======================================")
  }
}
