/*
 * Copyright (c) 2014.
 *
 * This file ChebeyshevApproximation5.scala is part of numrecip (numrecip)
 *
 *     numrecip / ChebeyshevApproximation5.scala is free software: you can redistribute it and/or modify
 *     it under the terms of the GNU General Public License as published by
 *     the Free Software Foundation, either version 3 of the License, or
 *     (at your option) any later version.
 *
 *     numrecip / ChebeyshevApproximation5.scala is distributed in the hope that it will be useful,
 *     but WITHOUT ANY WARRANTY; without even the implied warranty of
 *     MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 *     GNU General Public License for more details.
 *
 *     You should have received a copy of the GNU General Public License
 *     along with numrecip.  If not, see <http://www.gnu.org/licenses/>.
 */

package com.github.fons.nr.examples

import com.github.fons.nr.function.approximation._

/**
 * Created with IntelliJ IDEA.
 * User: fons
 * Date: 5/1/14
 * Time: 6:32 PM
 * To change this template use File | Settings | File Templates.
 */
object ChebeyshevApproximation5 {
  def run {
    val approx: ApproximationT = new Approximation(initialOrder = 40, maxIter = 5) with ChebClenshawEvalStrategy with HeuristicSolver

    println("approximations using barycentric interpolation and the heuristic solver")
    val x = 0.1234
    println("approximating exp on -1, 1")
    val b: Option[FunctionApproximation] = approx(scala.math.exp)
    println("approximating atan on -1,1")
    val c = approx(scala.math.atan)

    println("approx b : " + b)
    println("approx c : " + c)
    println("b == c", b == c, c == b)
    println("b == b", b == b)

    val approx2: ApproximationT = new Approximation(initialOrder = 40, maxIter = 5) with ChebClenshawEvalStrategy with HeuristicSolver
    val b2 = approx2(scala.math.exp)

    println("approx b2 : " + b2)
    println("b == b2 (s/b true)", b == b2, b2 == b)

    println("approx  : " + approx)
    println("approx2 : " + approx2)

    println(" approx == approx2 ", approx == approx2)

    val approx3 = new Approximation with ChebClenshawEvalStrategy with HeuristicSolver
    println("approx3 : " + approx3)
    println(" approx == approx3", approx == approx3)

  }
}
