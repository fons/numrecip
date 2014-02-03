/*
 * Copyright (c) 2014.
 *
 * This file OdeMemoizeBurlischStoer.scala is part of numrecip (numrecip)
 *
 *     numrecip / OdeMemoizeBurlischStoer.scala is free software: you can redistribute it and/or modify
 *     it under the terms of the GNU General Public License as published by
 *     the Free Software Foundation, either version 3 of the License, or
 *     (at your option) any later version.
 *
 *     numrecip / OdeMemoizeBurlischStoer.scala is distributed in the hope that it will be useful,
 *     but WITHOUT ANY WARRANTY; without even the implied warranty of
 *     MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 *     GNU General Public License for more details.
 *
 *     You should have received a copy of the GNU General Public License
 *     along with numrecip.  If not, see <http://www.gnu.org/licenses/>.
 */

package com.github.fons.nr.examples

import scala.math._
import com.github.fons.nr.ode.{Memoize, ExplicitRungeKutta, OdeSolver}
import com.github.fons.nr.ode.butcher.tableau.RKE56Tableau
import com.github.fons.nr.interpolation._
import scala.util.Success
import com.github.fons.nr.interpolation.Interpolator
import com.github.fons.nr.util.Accuracy

/**
 * Created with IntelliJ IDEA.
 * User: fons
 * Date: 1/30/14
 * Time: 6:34 PM
 * To change this template use File | Settings | File Templates.
 */
object OdeMemoizeBurlischStoer {
      def run () {

        def f(t: Double, x: Seq[Double]): Double = -Pi * x(1)
        def g(t: Double, x: Seq[Double]): Double = Pi * x(0)

        //exact solution
        def fx(t: Double) = cos(Pi * t)
        def gx(t: Double) = sin(Pi * t)

        val step = 0.01
        val init = (0.0, List(1.0,0.0))
        val ode = new OdeSolver(step, init, List(f,g)) with ExplicitRungeKutta with RKE56Tableau with Memoize
        val degr = 4
        val result = ode(1.25)
        result match {
          case Success(((tn, results), mem)) => {
            val ds = DataSet(mem.toList)
            ds.pp
            //println(ds)
            val cs = new {
              val degree = degr
              val accuracy = Accuracy()
            } with Interpolator(ds) with  PolynomialApproximation with BulirschStoerNevilleStrategy
            val x = 1.045
            val i = cs(x)
            println("degree : " + degr + " independent variable : " + x  + " interpolation result : " + i)
            println("exact result : " + fx(x) + " and " + gx(x))
          }
          case _ => println("error")
        }
      }

}
