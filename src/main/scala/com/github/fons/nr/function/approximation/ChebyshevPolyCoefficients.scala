/*
 * Copyright (c) 2014.
 *
 * This file ChebyshevPolyCoefficients.scala is part of numrecip (numrecip)
 *
 *     numrecip / ChebyshevPolyCoefficients.scala is free software: you can redistribute it and/or modify
 *     it under the terms of the GNU General Public License as published by
 *     the Free Software Foundation, either version 3 of the License, or
 *     (at your option) any later version.
 *
 *     numrecip / ChebyshevPolyCoefficients.scala is distributed in the hope that it will be useful,
 *     but WITHOUT ANY WARRANTY; without even the implied warranty of
 *     MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 *     GNU General Public License for more details.
 *
 *     You should have received a copy of the GNU General Public License
 *     along with numrecip.  If not, see <http://www.gnu.org/licenses/>.
 */

package com.github.fons.nr.function.approximation

import com.github.fons.nr.matrix.Matrix
import scala.annotation.tailrec
import com.github.fons.nr.util._

/**
 * Created with IntelliJ IDEA.
 * User: fons
 * Date: 4/18/14
 * Time: 6:42 PM
 * To change this template use File | Settings | File Templates.
 */

/*
  calculates the chebyshev coeffiecients
 */
trait ChebyshevPolyCoefficients {

  private
  def coeff_matrix(N: Int) = {
    val const0 = 1.0 / (N + 1.0)
    val const1 = 2.0 * const0
    val const = scala.math.Pi / (2.0 * (N + 1))
    Matrix.Zero(N + 1, N + 1).map((row: Int, col: Int, v: Double) => if (row == 0) const0 else const1 * math.cos(const * (1.0 + col * 2.0) * row))
  }

  @tailrec
  private
  def zeroes(k: Int, const: Double, N: Int, accum: List[Double]): List[Double] = {
    val value = scala.math.cos(const * (k + 1.0))
    k match {
      case 0 => value :: accum
      case k if k > 0 => zeroes(k - 2, const, N, value :: accum)
    }
  }

  private
  def sample(N: Int, func: (Double) => Option[Double]): Option[List[Double]] = {
    val const = scala.math.Pi / (2.0 * N + 2.0)
    val z = zeroes(2 * N, const, N, List())
    //println(z)
    listToOption(z.map(func))
  }


  def chebyshevCoefficients(N: Int, func: (Double) => Option[Double]): Option[List[Double]] = {
    sample(N, func).flatMap((coeff: List[Double]) => (coeff_matrix(N) * Matrix((coeff).map(List(_)))).coll(0).map(_.toList))
  }


}
