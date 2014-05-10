/*
 * Copyright (c) 2014.
 *
 * This file ChebPolyEvalStrategy.scala is part of numrecip (numrecip)
 *
 *     numrecip / ChebPolyEvalStrategy.scala is free software: you can redistribute it and/or modify
 *     it under the terms of the GNU General Public License as published by
 *     the Free Software Foundation, either version 3 of the License, or
 *     (at your option) any later version.
 *
 *     numrecip / ChebPolyEvalStrategy.scala is distributed in the hope that it will be useful,
 *     but WITHOUT ANY WARRANTY; without even the implied warranty of
 *     MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 *     GNU General Public License for more details.
 *
 *     You should have received a copy of the GNU General Public License
 *     along with numrecip.  If not, see <http://www.gnu.org/licenses/>.
 */

package com.github.fons.nr.function.approximation

import com.github.fons.nr.function.ChebyshevPolynomial
import scala.annotation.tailrec

/**
 * Created with IntelliJ IDEA.
 * User: fons
 * Date: 3/30/14
 * Time: 7:13 PM
 * To change this template use File | Settings | File Templates.
 */
trait ChebPolyEvalStrategy extends EvalStrategyT with ChebyshevPolyCoefficients with ChebyshevDerivative with ChebeyshevIntegral {

  @tailrec
  private
  def direct_calc(x: Double, coeff: List[Double], N: Int = 0, accum: Double = 0): Double = {
    coeff match {
      case List() => accum
      case (c :: cs) => direct_calc(x, cs, N + 1, accum + (c * ChebyshevPolynomial(N)(x)))
    }
  }

  override
  protected
  def isEqualEvalStrategy(other: Any) = other.isInstanceOf[ChebPolyEvalStrategy]

  override
  def coefficients(N: Int, func: (Double) => Option[Double]): Option[List[Double]] = chebyshevCoefficients(N, func)

  override
  def approximant(func: (Double) => Option[Double], order: Int): (Double) => Option[Double] = {
    val coeff = chebyshevCoefficients(order, func)
    (x: Double) => coeff.map(direct_calc(x, _))
  }

  override
  protected
  val evalStrategyString = evalName(this)

  override
  def derivative(coeff: List[Double]): Option[EvalStrategyResult] = {
    coeff.length match {
      case n if n > 0 => {
        val c1 = derivativeCoeff(n - 2, coeff.reverse, List(0.0, 0.0))
        val f1 = (x: Double) => Some(direct_calc(x, c1))
        Some(EvalStrategyResult(f1, c1))
      }
      case _ => None
    }
  }

  override
  def integral(x0: Double, coeff: List[Double]): Option[EvalStrategyResult] = {
    val c1 = integralCoeff(1, coeff, List(x0))
    val f1 = (x: Double) => Some(direct_calc(x, c1))
    Some(EvalStrategyResult(f1, c1))
  }

}
