/*
 * Copyright (c) 2014.
 *
 * This file ChebeyshevApproximation1.scala is part of numrecip (numrecip)
 *
 *     numrecip / ChebeyshevApproximation1.scala is free software: you can redistribute it and/or modify
 *     it under the terms of the GNU General Public License as published by
 *     the Free Software Foundation, either version 3 of the License, or
 *     (at your option) any later version.
 *
 *     numrecip / ChebeyshevApproximation1.scala is distributed in the hope that it will be useful,
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
 * Date: 3/31/14
 * Time: 6:24 PM
 * To change this template use File | Settings | File Templates.
 */

object ChebeyshevApproximation1 {

  def run {

    /*
     * Return an approximation function.
     * An approximation function takes a regular function and returns a function approximation.
     */
    val approx: ApproximationT = new Approximation with BarycentricInterpolation with HeuristicSolver

    println("approximations using barycentric interpolation and the heuristic solver")

    println("approximating exp on -1, 1")
    val b: Option[FunctionApproximation] = approx(scala.math.exp)

    println(b, b.map(_.coefficients), b.flatMap(_(0.0)))

    val x = 0.1234

    println("exacted exp(0.1234) : ", scala.math.exp(x), b.map(_(x)))
    println("approximating atan on -1,1")
    val b2 = approx(scala.math.atan)
    println(b2, b2.map(_.coefficients), b2.flatMap(_(0.0)))
    println("exacted atan(0.1234) : ", scala.math.atan(x), b2.map(_(x)))

  }
}
