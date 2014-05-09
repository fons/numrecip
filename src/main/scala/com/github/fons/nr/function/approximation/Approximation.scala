/*
 * Copyright (c) 2014.
 *
 * This file Approximation.scala is part of numrecip (numrecip)
 *
 *     numrecip / Approximation.scala is free software: you can redistribute it and/or modify
 *     it under the terms of the GNU General Public License as published by
 *     the Free Software Foundation, either version 3 of the License, or
 *     (at your option) any later version.
 *
 *     numrecip / Approximation.scala is distributed in the hope that it will be useful,
 *     but WITHOUT ANY WARRANTY; without even the implied warranty of
 *     MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 *     GNU General Public License for more details.
 *
 *     You should have received a copy of the GNU General Public License
 *     along with numrecip.  If not, see <http://www.gnu.org/licenses/>.
 */

package com.github.fons.nr.function.approximation

import com.github.fons.nr.util.Interval

/**
 * Created with IntelliJ IDEA.
 * User: fons
 * Date: 3/29/14
 * Time: 4:31 PM
 * To change this template use File | Settings | File Templates.
 */


//
// These values can't be too high; At ech iteration the set size is doubled, starting from the initial_order value.
// so the max size is 2 * (N_i-1 - 1) = N_i; N_0 = initial_order
//

class Approximation(val maxIter: Int = 18, val initialOrder: Int = 20) extends ApproximationT with EvalStrategyT with HeuristicSolverT {

  def <>(func: (Double) => Option[Double], interval: Interval = Interval(-1, 1), order: Option[Int] = None): Option[FunctionApproximation] = {
    val sfunc = ChebShift(func, interval.from, interval.to)
    order match {
      case Some(n) => coefficients(n, sfunc).map(FunctionApproximation(_, approximant(func, n), interval, func, Some(this)))
      case _ => heuristics(func, approximant, interval, initialOrder, maxIter) match {
        case Some((approx, n)) => coefficients(n, sfunc).map(FunctionApproximation(_, approx, interval, func, Some(this)))
        case _ => None
      }
    }
  }

  def derivative(v1: FunctionApproximation): Option[FunctionApproximation] = {
    derivative(v1.coefficients).map {
      case EvalStrategyResult(approx, coeff) => FunctionApproximation(coeff, approx, v1.interval, approx, Some(this))
    }
  }

  def integral(x0: Double, v1: FunctionApproximation): Option[FunctionApproximation] = {
    integral(x0, v1.coefficients).map {
      case EvalStrategyResult(approx, coeff) => FunctionApproximation(coeff, approx, v1.interval, approx, Some(this))
    }
  }

  final
  override
  def equals(other: Any) = {
    val that = other.asInstanceOf[Approximation]
    (that != null) && (isEqualEvalStrategy(that)) && isEqualHeuristicSolver(that) && (maxIter == that.maxIter) && (initialOrder == that.initialOrder)
  }

  override
  lazy
  val toString = "Approximation(maxIter = " + maxIter + ", initialOrder = " + initialOrder + "){" + evalStrategyString + "," + heuristicSolverString + "}@" + hashCode()


}

