/*
 * Copyright (c) 2014.
 *
 * This file ChebClenshawStrategy.scala is part of numrecip (numrecip)
 *
 *     numrecip / ChebClenshawStrategy.scala is free software: you can redistribute it and/or modify
 *     it under the terms of the GNU General Public License as published by
 *     the Free Software Foundation, either version 3 of the License, or
 *     (at your option) any later version.
 *
 *     numrecip / ChebClenshawStrategy.scala is distributed in the hope that it will be useful,
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
 * Date: 3/30/14
 * Time: 10:35 AM
 * To change this template use File | Settings | File Templates.
 */


trait ChebClenshawEvalStrategy extends EvalStrategyT with ChebyshevPolyCoefficients with ChebyshevDerivative with ChebeyshevIntegral {


  @tailrec
  private
  def clenshaw(x: Double, coeff: List[Double], b: List[Double] = List()): Double = {

    (coeff, b) match {

      case (_, List()) => {
        val r = coeff.reverse
        clenshaw(x, r.tail, List(r.head, 0.0))
      }

      case (List(c), _) => {
        val b1 = b.head
        val b2 = b.tail.head
        x * b1 - b2 + c
      }

      case (c :: cs, _) => {
        val b1 = b.head
        val b2 = b.tail.head
        val c = coeff.head
        clenshaw(x, coeff.tail, (2.0 * x * b1 - b2 + c) :: b)
      }

    }
  }

  override
  protected
  def isEqualEvalStrategy(other: Any) = other.isInstanceOf[ChebClenshawEvalStrategy]

  override
  protected
  val evalStrategyString = evalName(this)

  override
  def coefficients(N: Int, func: (Double) => Option[Double]): Option[List[Double]] = chebyshevCoefficients(N, func)

  override
  def approximant(func: (Double) => Option[Double], order: Int): (Double) => Option[Double] = {
    val coeff = chebyshevCoefficients(order, func)
    (x: Double) => coeff.map(clenshaw(x, _))
  }

  override
  def derivative(coeff: List[Double]): Option[EvalStrategyResult] = {
    coeff.length match {
      case n if n > 0 => {
        val c1 = derivativeCoeff(n - 2, coeff.reverse, List(0.0, 0.0))
        val f1 = (x: Double) => Some(clenshaw(x, c1))
        Some(EvalStrategyResult(f1, c1))
      }
      case _ => None
    }
  }

  override
  def integral(x0: Double, coeff: List[Double]): Option[EvalStrategyResult] = {
    val c1 = integralCoeff(1, coeff, List(x0))
    val f1 = (x: Double) => Some(clenshaw(x, c1))
    Some(EvalStrategyResult(f1, c1))
  }
}
