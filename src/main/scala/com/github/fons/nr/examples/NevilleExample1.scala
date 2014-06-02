/*
 * Copyright (c) 2014.
 *
 * This file NevilleExample1.scala is part of numrecip (numrecip)
 *
 *     numrecip / NevilleExample1.scala is free software: you can redistribute it and/or modify
 *     it under the terms of the GNU General Public License as published by
 *     the Free Software Foundation, either version 3 of the License, or
 *     (at your option) any later version.
 *
 *     numrecip / NevilleExample1.scala is distributed in the hope that it will be useful,
 *     but WITHOUT ANY WARRANTY; without even the implied warranty of
 *     MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 *     GNU General Public License for more details.
 *
 *     You should have received a copy of the GNU General Public License
 *     along with numrecip.  If not, see <http://www.gnu.org/licenses/>.
 */

package com.github.fons.nr.examples

import com.github.fons.nr.interpolation.{DataSet, LagrangeNevilleStrategy, PolynomialApproximation, Interpolator}

/**
 * Created with IntelliJ IDEA.
 * User: fons
 * Date: 1/27/14
 * Time: 3:07 PM
 * To change this template use File | Settings | File Templates.
 */

object NevilleExample1 {
  def run {
    val xvals = Vector(0.1, 0.2, 0.3, 0.4, 0.5)
    val yvals = Vector(-1.6228, -0.8218, -0.3027, 0.1048, 0.4542)
    val x = 0.15
    //  val xvals = Vector(2.0,3.0,5.0,8.0)
    //  val yvals = Vector(3.0,8.0,4.0,2.0)
    //  val x = 4.0
    val results = Map((0 -> -1.6228), (1 -> -1.22230), (2 -> -1.18706), (3 -> -1.17642), (4 -> -1.17186), (5 -> -1.17186))
    for (degr <- Range(0, 6)) {
      val inter = new {
        val degree = degr
      } with Interpolator(DataSet(xvals, yvals)) with PolynomialApproximation with LagrangeNevilleStrategy
      println(inter)
      val resx = inter(x)
      println("degree : " + degr + " x : " + x + "result : " + resx + " actual : " + results(degr))
    }
  }
}
