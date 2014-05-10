/*
 * Copyright (c) 2014.
 *
 * This file ChebyshevPolynomialExpansion.scala is part of numrecip (numrecip)
 *
 *     numrecip / ChebyshevPolynomialExpansion.scala is free software: you can redistribute it and/or modify
 *     it under the terms of the GNU General Public License as published by
 *     the Free Software Foundation, either version 3 of the License, or
 *     (at your option) any later version.
 *
 *     numrecip / ChebyshevPolynomialExpansion.scala is distributed in the hope that it will be useful,
 *     but WITHOUT ANY WARRANTY; without even the implied warranty of
 *     MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 *     GNU General Public License for more details.
 *
 *     You should have received a copy of the GNU General Public License
 *     along with numrecip.  If not, see <http://www.gnu.org/licenses/>.
 */

package com.github.fons.nr.function.approximation

import com.github.fons.nr.util.{Interval, toPartialFunction}


/**
 * Created with IntelliJ IDEA.
 * User: fons
 * Date: 4/14/14
 * Time: 6:56 PM
 * To change this template use File | Settings | File Templates.
 */
object ChebyshevPolynomialExpansion extends ChebClenshawEvalStrategy with HeuristicSolver {

  def apply(func: (Double) => Double, interval: Interval = Interval(-1, 1), order: Option[Int] = None): Option[FunctionApproximation] = {
    <>(toPartialFunction(interval, func), interval, order)
  }

  def <>(func: (Double) => Option[Double], interval: Interval = Interval(-1, 1), order: Option[Int] = None): Option[FunctionApproximation] = {
    order match {
      case Some(n) => chebyshevCoefficients(n, func).map(FunctionApproximation(_, approximant(func, n), interval, func))
      case _ => heuristics(func, approximant, interval, initialOrder = 20, maxIter = 5) match {
        case Some((approx, n)) => chebyshevCoefficients(n, func).map(FunctionApproximation(_, approx, interval, func))
        case _ => None
      }
    }
  }
}
