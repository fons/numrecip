/*
 * Copyright (c) 2014.
 *
 * This file BurlirschStoerExample2.scala is part of numrecip (numrecip)
 *
 *     numrecip / BurlirschStoerExample2.scala is free software: you can redistribute it and/or modify
 *     it under the terms of the GNU General Public License as published by
 *     the Free Software Foundation, either version 3 of the License, or
 *     (at your option) any later version.
 *
 *     numrecip / BurlirschStoerExample2.scala is distributed in the hope that it will be useful,
 *     but WITHOUT ANY WARRANTY; without even the implied warranty of
 *     MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 *     GNU General Public License for more details.
 *
 *     You should have received a copy of the GNU General Public License
 *     along with numrecip.  If not, see <http://www.gnu.org/licenses/>.
 */

package com.github.fons.nr.examples

import scala.math._
import com.github.fons.nr.interpolation.{BulirschStoerNevilleStrategy, PolynomialApproximation, DataSet, Interpolator}

/**
 * Created with IntelliJ IDEA.
 * User: fons
 * Date: 1/30/14
 * Time: 6:51 PM
 * To change this template use File | Settings | File Templates.
 */
object BulirschStoerExample2 {
       def run() {

         def f (x:Double):Double = cos(Pi*x)
         val xvals = (for (i <- Range(0, 6)) yield 0.68 + i*0.01).toVector
         val yvals = (xvals.map((x)=>f(x))).toVector

         val degree = 5
         val x = 0.705
         val inter = new {val Degree = degree} with Interpolator(DataSet(xvals, yvals)) with PolynomialApproximation with BulirschStoerNevilleStrategy
         println(inter)
         val resx = inter(x)
         println("degree : " + degree + "  input : " + x + " result : " + resx + " exact : " + f(x))



       }
}
