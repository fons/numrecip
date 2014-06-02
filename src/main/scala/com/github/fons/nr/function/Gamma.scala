/*
 * Copyright (c) 2014.
 *
 * This file Gamma.scala is part of numrecip (numrecip)
 *
 *     numrecip / Gamma.scala is free software: you can redistribute it and/or modify
 *     it under the terms of the GNU General Public License as published by
 *     the Free Software Foundation, either version 3 of the License, or
 *     (at your option) any later version.
 *
 *     numrecip / Gamma.scala is distributed in the hope that it will be useful,
 *     but WITHOUT ANY WARRANTY; without even the implied warranty of
 *     MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 *     GNU General Public License for more details.
 *
 *     You should have received a copy of the GNU General Public License
 *     along with numrecip.  If not, see <http://www.gnu.org/licenses/>.
 */

package com.github.fons.nr.function

/**
 * Created with IntelliJ IDEA.
 * User: fons
 * Date: 5/19/14
 * Time: 6:48 PM
 * To change this template use File | Settings | File Templates.
 */
// from  http://en.wikipedia.org/wiki/Lanczos_approximation

/*
    from cmath import *

    # Coefficients used by the GNU Scientific Library
    g = 7
    p = [0.99999999999980993, 676.5203681218851, -1259.1392167224028,
    771.32342877765313, -176.61502916214059, 12.507343278686905,
    -0.13857109526572012, 9.9843695780195716e-6, 1.5056327351493116e-7]

    def gamma(z):
    z = complex(z)
    # Reflection formula
    if z.real < 0.5:
    return pi / (sin(pi*z) * gamma(1-z))
    else:
    z -= 1
    x = p[0]
    for i in range(1, g+2):
          x += p[i]/(z+i)
    t = z + g + 0.5
     return sqrt(2*pi) * t**(z+0.5) * exp(-t) * x
*/
/*
    static double cof[6]={76.18009172947146,-86.50532032941677,
    24.01409824083091,-1.231739572450155,
    0.1208650973866179e-2,-0.5395239384953e-5}; int j;
  */


import scala.math.{log, exp, Pi, pow, sqrt, sin}

object Gamma {
  private
  val p = Vector(
    0.99999999999980993,
    676.5203681218851,
    -1259.1392167224028,
    771.32342877765313,
    -176.61502916214059,
    12.507343278686905,
    -0.13857109526572012,
    9.9843695780195716e-6,
    1.5056327351493116e-7)

  private
  val g = 7

  private
  val ser = 1.000000000190015

  private
  val logp = Vector(76.18009172947146,
    -86.50532032941677,
    24.01409824083091,
    -1.231739572450155,
    0.1208650973866179e-2,
    -0.5395239384953e-5)

  private
  def log_calc(x: Double) = {
    val r: Vector[Double] = (for (index <- Range(0, 6)) yield {
      logp(index) / (x + index + 1)
    }).toVector
    val t2 = (ser +: r).reduce(_ + _)
    (x + 0.5) * log(x + 5.5) - (x + 5.5) + log(sqrt(2 * Pi) * t2 / x)
  }

  private
  def calc(z: Double) = {
    val v = (for (index <- Range(1, g + 2)) yield {
      p(index) / (z + index)
    }).toVector
    val x = (p(0) +: v).reduce(_ + _)

    val t = z + g + 0.5
    (sqrt(2 * Pi) * pow(t, z + 0.5) * exp(-t) * x)
  }

  def apply(z: Double): Double = {
    if (z < 0.5) {
      Pi / (sin(Pi * z) * apply(1.0 - z))
    }
    else calc(z - 1)
  }

  def ln(x: Double) = log_calc(x)

}
