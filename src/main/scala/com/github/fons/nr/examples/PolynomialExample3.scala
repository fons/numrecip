/*
 * Copyright (c) 2014.
 *
 * This file PolynomialExample3.scala is part of numrecip (numrecip)
 *
 *     numrecip / PolynomialExample3.scala is free software: you can redistribute it and/or modify
 *     it under the terms of the GNU General Public License as published by
 *     the Free Software Foundation, either version 3 of the License, or
 *     (at your option) any later version.
 *
 *     numrecip / PolynomialExample3.scala is distributed in the hope that it will be useful,
 *     but WITHOUT ANY WARRANTY; without even the implied warranty of
 *     MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 *     GNU General Public License for more details.
 *
 *     You should have received a copy of the GNU General Public License
 *     along with numrecip.  If not, see <http://www.gnu.org/licenses/>.
 */

package com.github.fons.nr.examples

import com.github.fons.nr.func.PowerSeries

/**
 * Created with IntelliJ IDEA.
 * User: fons
 * Date: 3/19/14
 * Time: 6:49 PM
 * To change this template use File | Settings | File Templates.
 */
object PolynomialExample3 {
  private
  def banner = println("\n------------------------------------------------------------------------------------------\n")

  def run {

    /*
    * Derivative of a power series
     */
    banner
    println("derivative of a power series")
    val p2 = PowerSeries(List(10.0, 0.0, 9.0, 1.0, 2.0))

    println("power series : " + p2 + "; coefficients : " + p2.coeff + " value @ 2 :" + p2(2.0))
    println("first order derivative : " + p2.deriv + "; coefficients : " + p2.deriv.coeff + "  value @ 2 : " + p2.deriv(2.0))
    println("second order derivative : " + p2.deriv(2) + "; coefficients : " + p2.deriv(2).coeff + "  value @ 2 : " + p2.deriv(2)(2.0))
    println("second order derivative (using ∂) : " + p2.∂(2) + "; coefficients : " + p2.∂(2).coeff + "  value @ 2 : " + p2.∂(2)(2.0))

    banner
    /*
     * Integral of a power series
     */

    val p3 = p2 ∫ (2.0)
    println("integral ; at 2.0 : " + p3 + "; coefficients : " + p3.coeff + "  value @ 2 : " + p3.deriv(2.0))
    println("derivative of integral : " + p3.deriv.coeff)

    banner
    /*
      *
     */

    println("power of a power series")
    val p4 = p2 ^ (5)
    println("p2^4 :" + p4 + " @2 : " + p4(2.0))
    println("5 times p2 @ 2.0 : " + (p2 * p2 * p2 * p2 * p2)(2.0))
    println("p2(2.0)^5 : " + p2(2.0).map(scala.math.pow(_, 5.0)))

  }


}
