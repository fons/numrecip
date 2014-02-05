/*
 * Copyright (c) 2014.
 *
 * This file OdeMemoizeExample.scala is part of numrecip (numrecip)
 *
 *     numrecip / OdeMemoizeExample.scala is free software: you can redistribute it and/or modify
 *     it under the terms of the GNU General Public License as published by
 *     the Free Software Foundation, either version 3 of the License, or
 *     (at your option) any later version.
 *
 *     numrecip / OdeMemoizeExample.scala is distributed in the hope that it will be useful,
 *     but WITHOUT ANY WARRANTY; without even the implied warranty of
 *     MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 *     GNU General Public License for more details.
 *
 *     You should have received a copy of the GNU General Public License
 *     along with numrecip.  If not, see <http://www.gnu.org/licenses/>.
 */

package com.github.fons.nr.examples

import scala.math._
import com.github.fons.nr.ode.{OdeResult, Memoize, ExplicitRungeKutta, OdeSolver}
import com.github.fons.nr.ode.butcher.tableau.RKE56Tableau
import scala.util.Success
import com.github.fons.nr.util.Accuracy
import com.github.fons.nr.interpolation.{NormalSpline, CubicSpline, BulirschStoerNevilleStrategy, PolynomialApproximation}
import com.github.fons.nr.matrix.LUSolver

/**
 * Created with IntelliJ IDEA.
 * User: fons
 * Date: 2/5/14
 * Time: 5:06 PM
 * To change this template use File | Settings | File Templates.
 */
object OdeMemoizeExample {
      def run {

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
          case Success(odeResult) => {
            odeResult.dataSet.pp()
            val res = new {
              val degree = degr
              val accuracy = Accuracy()
            } with OdeResult(odeResult)  with  PolynomialApproximation with BulirschStoerNevilleStrategy
            println(res.last, res(res.last), res())
            println(res(1.045))
            println(res(1.16))
            println(res(10.045))
            println(res)
            val res1 = new OdeResult(odeResult)  with  CubicSpline with NormalSpline with LUSolver
            println(res1.last, res(res.last), res())
            println(res1(1.045))
            println("exact : " + fx(1.045) + " " + gx(1.045))
            println(res1(1.16))
            println("exact : " + fx(1.16) + " " + gx(1.16))
            println(res1(10.045))
            println(res1)
          }
          case _ => println("error")
        }

      }
}
