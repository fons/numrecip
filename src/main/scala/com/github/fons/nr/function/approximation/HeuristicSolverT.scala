/*
 * Copyright (c) 2014.
 *
 * This file HeuristicSolverT.scala is part of numrecip (numrecip)
 *
 *     numrecip / HeuristicSolverT.scala is free software: you can redistribute it and/or modify
 *     it under the terms of the GNU General Public License as published by
 *     the Free Software Foundation, either version 3 of the License, or
 *     (at your option) any later version.
 *
 *     numrecip / HeuristicSolverT.scala is distributed in the hope that it will be useful,
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
 * Date: 4/18/14
 * Time: 6:19 PM
 * To change this template use File | Settings | File Templates.
 */
trait HeuristicSolverT {
  protected
  type approxT = ((Double) => Option[Double], Int) => (Double) => Option[Double]

  protected
  def heuristicSolverName[A](a: A)(implicit m: Manifest[A]) = m.toString

  protected
  val heuristicSolverString = heuristicSolverName(this)

  protected
  def isEqualHeuristicSolver(other: Any): Boolean = other.isInstanceOf[HeuristicSolverT]

  def heuristics(func: (Double) => Option[Double], app: approxT, interval: Interval, initialOrder: Int, maxIter: Int): Option[((Double) => Option[Double], Int)] = None
}
