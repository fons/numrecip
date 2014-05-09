/*
 * Copyright (c) 2014.
 *
 * This file ChebeyshevIntegral.scala is part of numrecip (numrecip)
 *
 *     numrecip / ChebeyshevIntegral.scala is free software: you can redistribute it and/or modify
 *     it under the terms of the GNU General Public License as published by
 *     the Free Software Foundation, either version 3 of the License, or
 *     (at your option) any later version.
 *
 *     numrecip / ChebeyshevIntegral.scala is distributed in the hope that it will be useful,
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
 * Date: 5/9/14
 * Time: 6:26 PM
 * To change this template use File | Settings | File Templates.
 */
trait ChebeyshevIntegral {

  private
  def assign_factor(x: (Double, Int)) = {
    val (l, nr) = x
    val r = nr + 1
    ((r / 2) * 2 == r) match {
      case false => 0.0
      case true => scala.math.pow(-1, (r / 2)) * l
    }

  }

  private
  def sum_it(l: List[Double]): Double = l.zipWithIndex.map(assign_factor).reduce(_ + _)

  @tailrec
  final
  def integralCoeff(N: Int, coeff: List[Double], accum: List[Double]): List[Double] = {
    (N, coeff) match {
      case (_, List()) => {
        val naccum = accum.reverse
        (naccum.head - sum_it(naccum.tail)) :: naccum.tail
      }
      case (1, x0 :: x1 :: x2 :: xs) => integralCoeff(N + 1, coeff.tail, 0.5 * (2.0 * x0 - x2) :: accum)
      case (i, x0 :: x1 :: List()) => integralCoeff(N + 1, coeff.tail, x0 / (2.0 * i) :: accum)
      case (i, x0 :: x1 :: x2 :: xs) => integralCoeff(N + 1, coeff.tail, (x0 - x2) / (2.0 * i) :: accum)
      case (i, x0 :: List()) => integralCoeff(N + 1, coeff.tail, x0 / (2.0 * i) :: accum)
    }
  }


}
