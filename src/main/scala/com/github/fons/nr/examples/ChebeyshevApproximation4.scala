/*
 * Copyright (c) 2014.
 *
 * This file ChebeyshevApproximation4.scala is part of numrecip (numrecip)
 *
 *     numrecip / ChebeyshevApproximation4.scala is free software: you can redistribute it and/or modify
 *     it under the terms of the GNU General Public License as published by
 *     the Free Software Foundation, either version 3 of the License, or
 *     (at your option) any later version.
 *
 *     numrecip / ChebeyshevApproximation4.scala is distributed in the hope that it will be useful,
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
 * Date: 4/30/14
 * Time: 6:53 PM
 * To change this template use File | Settings | File Templates.
 */
object ChebeyshevApproximation4 {
  def run {
    /*
       * Return an approximation function.
       * An approximation function takes a regular function and returns a function approximation.
       *
       * Add/subtract/mutiply/divide two function approximations
       *
       */

    val approx: ApproximationT = new Approximation(initialOrder = 40, maxIter = 5) with ChebClenshawEvalStrategy with HeuristicSolver

    println("approximations using chebyshev-clenshaw interpolation and the heuristic solver")
    println("performing arithmetic")

    val x = 0.1234
    println("approximating exp on -1, 1")
    val b: Option[FunctionApproximation] = approx(scala.math.exp)
    println("approximating atan on -1,1")
    val c = approx(scala.math.atan)
    println("----------------------------------")
    val y3 = scala.math.exp(x) / scala.math.atan(x)
    val bp3 = b.flatMap(_ </> c)
    println("division of exp and atan at : " + x + " exact : " + y3 + "  approx : " + bp3.flatMap(_(x)))

    val y0 = scala.math.exp(x) + scala.math.atan(x)
    val bp0 = b.flatMap(_ <+> c)
    println("sum of exp and atan at : " + x + " exact : " + y0 + "  approx : " + bp0.flatMap(_(x)))

    val y1 = scala.math.exp(x) - scala.math.atan(x)
    val bp1 = b.flatMap(_ <-> c)
    println("diff (minus) of exp and atan at : " + x + " exact : " + y1 + "  approx : " + bp1.flatMap(_(x)))

    val y2 = scala.math.exp(x) * scala.math.atan(x)
    val bp2 = b.flatMap(_ <*> c)
    println("product of exp and atan at : " + x + " exact : " + y2 + "  approx : " + bp2.flatMap(_(x)))

  }
}
