/*
 * Copyright (c) 2014.
 *
 * This file ChebeyshevApproximation6.scala is part of numrecip (numrecip)
 *
 *     numrecip / ChebeyshevApproximation6.scala is free software: you can redistribute it and/or modify
 *     it under the terms of the GNU General Public License as published by
 *     the Free Software Foundation, either version 3 of the License, or
 *     (at your option) any later version.
 *
 *     numrecip / ChebeyshevApproximation6.scala is distributed in the hope that it will be useful,
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
 * Date: 5/5/14
 * Time: 6:37 PM
 * To change this template use File | Settings | File Templates.
 */
object ChebeyshevApproximation6 {
  def run {
    val approx: ApproximationT = new Approximation with ChebClenshawEvalStrategy with HeuristicSolver

    println("approximations using barycentric interpolation and the heuristic solver")
    val x = 0.1234
    println("approximating exp on -1, 1")
    val b: Option[FunctionApproximation] = approx(scala.math.exp)

    val c: Option[FunctionApproximation] = b match {
      case Some(fa) => approx <> (fa)
      case _ => None
    }
    println("exp approximation : " + b)
    println("appproximation of the exp approximation : " + c)

    println("value at x = " + x + " using approx : " + b.map(_(x)))
    println("value at x = " + x + " approx of approx : " + c.map(_(x)))
    println("are the two approx equal ? => " + c == b)


  }
}