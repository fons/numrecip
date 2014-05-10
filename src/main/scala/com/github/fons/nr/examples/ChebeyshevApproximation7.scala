/*
 * Copyright (c) 2014.
 *
 * This file ChebeyshevApproximation7.scala is part of numrecip (numrecip)
 *
 *     numrecip / ChebeyshevApproximation7.scala is free software: you can redistribute it and/or modify
 *     it under the terms of the GNU General Public License as published by
 *     the Free Software Foundation, either version 3 of the License, or
 *     (at your option) any later version.
 *
 *     numrecip / ChebeyshevApproximation7.scala is distributed in the hope that it will be useful,
 *     but WITHOUT ANY WARRANTY; without even the implied warranty of
 *     MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 *     GNU General Public License for more details.
 *
 *     You should have received a copy of the GNU General Public License
 *     along with numrecip.  If not, see <http://www.gnu.org/licenses/>.
 */

package com.github.fons.nr.examples

import com.github.fons.nr.function.approximation.{ChebPolyEvalStrategy, HeuristicSolver, BarycentricInterpolation, Approximation}

/**
 * Created with IntelliJ IDEA.
 * User: fons
 * Date: 5/9/14
 * Time: 7:18 PM
 * To change this template use File | Settings | File Templates.
 */
object ChebeyshevApproximation7 {
  def run() {
    /*
     *
     */
    println("differention and integration of exp")
    val app1 = new Approximation with BarycentricInterpolation with HeuristicSolver
    val app2 = new Approximation with ChebPolyEvalStrategy with HeuristicSolver
    val fa1 = app1(scala.math.exp)
    val fa2 = app2(scala.math.exp)
    val fa3 = fa1.flatMap(app2.<>(_))
    val x0 = 0.456788

    val x = fa1.flatMap(_(x0))
    val y = fa3.flatMap(_(x0))
    println(x, y, scala.math.exp(x0))

    println(fa2)
    println(fa2.flatMap(_(0.5)))
    val der = fa2.flatMap(_.deriv)
    println(der)
    println(der.flatMap(_(0.5)), 1.0 / (1.0 + 0.5 * 0.5))
    println(fa2.map(_.coefficients))
    println(der.map(_.coefficients))

    val d2 = der.flatMap(_.inter(1.0))
    println(d2.map(_.coefficients))

    val f3 = fa1.flatMap(app2 <> (_))
    val dr3 = f3.flatMap(_.deriv)
    println(dr3.map(_.coefficients))
  }
}
