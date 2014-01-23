/*
 * Copyright (c) 2014.
 *
 * This file OdeExample1.scala is part of numrecip (numrecip)
 *
 *     numrecip / OdeExample1.scala is free software: you can redistribute it and/or modify
 *     it under the terms of the GNU General Public License as published by
 *     the Free Software Foundation, either version 3 of the License, or
 *     (at your option) any later version.
 *
 *     numrecip / OdeExample1.scala is distributed in the hope that it will be useful,
 *     but WITHOUT ANY WARRANTY; without even the implied warranty of
 *     MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 *     GNU General Public License for more details.
 *
 *     You should have received a copy of the GNU General Public License
 *     along with numrecip.  If not, see <http://www.gnu.org/licenses/>.
 */

package com.github.fons.nr.examples

import scala.math._
import scala.util.{Success, Try}

import com.github.fons.nr.ode.{Solver, OdeSolverT, Factory}

/**
 * Created with IntelliJ IDEA.
 * User: fons
 * Date: 1/13/14
 * Time: 6:12 AM
 * To change this template use File | Settings | File Templates.
 */
object OdeExample1 {
      def run {
        /// ode to solve
        def f(t: Double, x: Double*): Double = -Pi * x(1)
        def g(t: Double, x: Double*): Double = Pi * x(0)
        //exact solution
        def fx(t: Double) = cos(Pi * t)
        def gx(t: Double) = sin(Pi * t)

        /**
        // Method    : Solver.RKE56; embedded Rung-Kutta
        // step size : 0.2
        // initial conditions : (0.0, List(1.0, 0.0))
        //                      0.0 : initial independent variable
        // set of ode's       : List(f,g)
        // accuracy           : 0.0001;
        // Because an accuracy is specified an adaptive step method will be used.
        **/

        val ode : Try[OdeSolverT] = Factory(Solver.RKE56, 0.2, (0.0, List(1.0, 0.0)), List(f, g), 0.00001)
        ode match {
          case Success(ode) => {
            ode(0.15) match {
              case Success(((tn,results:List[Double]), _)) => {
                println("method : " + Solver.RKE56 + " for " + tn)
                println("returned : " + results(0) + ", " + results(1))
                println("exact    : " + fx(tn) + ", " + gx(tn))
              }
              case _ => println("failed")
            }
          }
          case _ => println("failed")
        }

      }
}
