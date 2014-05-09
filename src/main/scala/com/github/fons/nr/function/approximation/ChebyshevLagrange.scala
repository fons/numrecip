/*
 * Copyright (c) 2014.
 *
 * This file ChebyshevLagrange.scala is part of numrecip (numrecip)
 *
 *     numrecip / ChebyshevLagrange.scala is free software: you can redistribute it and/or modify
 *     it under the terms of the GNU General Public License as published by
 *     the Free Software Foundation, either version 3 of the License, or
 *     (at your option) any later version.
 *
 *     numrecip / ChebyshevLagrange.scala is distributed in the hope that it will be useful,
 *     but WITHOUT ANY WARRANTY; without even the implied warranty of
 *     MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 *     GNU General Public License for more details.
 *
 *     You should have received a copy of the GNU General Public License
 *     along with numrecip.  If not, see <http://www.gnu.org/licenses/>.
 */

package com.github.fons.nr.function.approximation


import com.github.fons.nr.util.Interval
import scala.Double

/**
 * Created with IntelliJ IDEA.
 * User: fons
 * Date: 4/16/14
 * Time: 6:35 PM
 * To change this template use File | Settings | File Templates.
 **/

object ChebyshevLagrange extends BarycentricInterpolation with HeuristicSolver {


  def <>(func: (Double) => Option[Double], interval: Interval = Interval(-1, 1), order: Option[Int] = None): Option[FunctionApproximation] = {
    order match {
      case Some(n) => coefficients(n, func).map(FunctionApproximation(_, approximant(func, n), interval, func))
      case _ => heuristics(func, approximant, interval, maxIter = 20, initialOrder = 20) match {
        case Some((approx, n)) => coefficients(n, func).map(FunctionApproximation(_, approx, interval, func))
        case _ => None
      }
    }
  }

}
