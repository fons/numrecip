/*
 * Copyright (c) 2014.
 *
 * This file ChebyshevDerivative.scala is part of numrecip (numrecip)
 *
 *     numrecip / ChebyshevDerivative.scala is free software: you can redistribute it and/or modify
 *     it under the terms of the GNU General Public License as published by
 *     the Free Software Foundation, either version 3 of the License, or
 *     (at your option) any later version.
 *
 *     numrecip / ChebyshevDerivative.scala is distributed in the hope that it will be useful,
 *     but WITHOUT ANY WARRANTY; without even the implied warranty of
 *     MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 *     GNU General Public License for more details.
 *
 *     You should have received a copy of the GNU General Public License
 *     along with numrecip.  If not, see <http://www.gnu.org/licenses/>.
 */

package com.github.fons.nr.function.approximation

import scala.annotation.tailrec

/**
 * Created with IntelliJ IDEA.
 * User: fons
 * Date: 5/8/14
 * Time: 9:24 PM
 * To change this template use File | Settings | File Templates.
 */
trait ChebyshevDerivative {

  @tailrec
  final
  def derivativeCoeff(N: Int, rev_coeff: List[Double], accum: List[Double]): List[Double] = {
    N match {
      case 0 => {
        val ciprime = accum.tail.head
        val ci = rev_coeff.head
        val c0 = 0.5 * (ciprime + 2.0 * ci)
        //println("*) ", N, ci, ciprime, c0)
        c0 :: accum
      }

      case n if n > 0 => {
        val ciprime = accum.tail.head
        val ci = rev_coeff.head
        val cn = 2.0 * ci * (N + 1) + ciprime
        //println("*) ", N, ci, ciprime, cn)
        derivativeCoeff(n - 1, rev_coeff.tail, cn :: accum)
      }

    }
  }

}
