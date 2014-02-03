/*
 * Copyright (c) 2014.
 *
 * This file BulirschStoerExample1.scala is part of numrecip (numrecip)
 *
 *     numrecip / BulirschStoerExample1.scala is free software: you can redistribute it and/or modify
 *     it under the terms of the GNU General Public License as published by
 *     the Free Software Foundation, either version 3 of the License, or
 *     (at your option) any later version.
 *
 *     numrecip / BulirschStoerExample1.scala is distributed in the hope that it will be useful,
 *     but WITHOUT ANY WARRANTY; without even the implied warranty of
 *     MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 *     GNU General Public License for more details.
 *
 *     You should have received a copy of the GNU General Public License
 *     along with numrecip.  If not, see <http://www.gnu.org/licenses/>.
 */

package com.github.fons.nr.examples

import com.github.fons.nr.interpolation.{BulirschStoerNevilleStrategy, PolynomialApproximation, DataSet, Interpolator}
import com.github.fons.nr.util.Accuracy

/**
 * Created with IntelliJ IDEA.
 * User: fons
 * Date: 1/30/14
 * Time: 6:28 PM
 * To change this template use File | Settings | File Templates.
 */
object BulirschStoerExample1 {
      def run () {
        val xvals = Vector(2.0,3.0,5.0,8.0)
        val yvals = Vector(3.0,8.0,4.0,2.0)

        val degr = 3
        val x = 4
        val inter = new {val degree   = degr
                         val accuracy = Accuracy() } with Interpolator(DataSet(xvals, yvals)) with PolynomialApproximation with BulirschStoerNevilleStrategy
        println(inter)
        val resx = inter(x)
        println("degree : " + degr + "  input : " + x + " result : " + resx)
      }
}
