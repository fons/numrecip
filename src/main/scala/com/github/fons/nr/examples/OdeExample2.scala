/*
 * Copyright (c) 2014.
 *
 * This file OdeExample2.scala is part of numrecip (numrecip)
 *
 *     numrecip / OdeExample2.scala is free software: you can redistribute it and/or modify
 *     it under the terms of the GNU General Public License as published by
 *     the Free Software Foundation, either version 3 of the License, or
 *     (at your option) any later version.
 *
 *     numrecip / OdeExample2.scala is distributed in the hope that it will be useful,
 *     but WITHOUT ANY WARRANTY; without even the implied warranty of
 *     MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 *     GNU General Public License for more details.
 *
 *     You should have received a copy of the GNU General Public License
 *     along with numrecip.  If not, see <http://www.gnu.org/licenses/>.
 */

package com.github.fons.nr.examples

/**
 * Created with IntelliJ IDEA.
 * User: fons
 * Date: 1/13/14
 * Time: 6:16 PM
 * To change this template use File | Settings | File Templates.
 */

import scala.math._
import scala.util.{Success, Try}

import com.github.fons.nr.ode.{Solver, OdeSolverT, Factory, OdeResult}

object OdeExample2 {
  def run {
    /// ode to solve
    def f(t: Double, x: Double*): Double = -Pi * x(1)
    def g(t: Double, x: Double*): Double = Pi * x(0)
    //exact solution
    def fx(t: Double) = cos(Pi * t)
    def gx(t: Double) = sin(Pi * t)

    /**
        // Method    : Solver.EU; Euler method
        // step size : 0.01
        // initial conditions : (0.0, List(1.0, 0.0))
        //                      0.0 : initial independent variable
        // set of ode's       : List(f,g)
        //
      **/

    val ode : Try[OdeSolverT] = Factory(Solver.EU, 0.01, (0.0, List(1.0, 0.0)), List(f, g))
    ode match {
      case Success(ode) => {
        ode(0.15) match {
          case Success(result) => {
            println("method : " + Solver.EU + " for " + result.first + " to " + result.last)
            println("returned : " + result.tail(0) + ", " + result.tail(1))
            println("exact    : " + fx(result.last) + ", " + gx(result.last))
          }
          case _ => println("solver failed")
        }
      }
      case _ => println("failed to a ode engine")
    }

  }

}
