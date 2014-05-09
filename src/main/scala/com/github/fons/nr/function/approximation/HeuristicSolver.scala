/*
 * Copyright (c) 2014.
 *
 * This file HeuristicCoefficientDetermination.scala is part of numrecip (numrecip)
 *
 *     numrecip / HeuristicCoefficientDetermination.scala is free software: you can redistribute it and/or modify
 *     it under the terms of the GNU General Public License as published by
 *     the Free Software Foundation, either version 3 of the License, or
 *     (at your option) any later version.
 *
 *     numrecip / HeuristicCoefficientDetermination.scala is distributed in the hope that it will be useful,
 *     but WITHOUT ANY WARRANTY; without even the implied warranty of
 *     MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 *     GNU General Public License for more details.
 *
 *     You should have received a copy of the GNU General Public License
 *     along with numrecip.  If not, see <http://www.gnu.org/licenses/>.
 */

package com.github.fons.nr.function.approximation

import scala.annotation.tailrec
import com.github.fons.nr.util.Interval
import com.github.fons.nr.util.liftedSum

/**
 * Created with IntelliJ IDEA.
 * User: fons
 * Date: 4/17/14
 * Time: 6:33 PM
 * To change this template use File | Settings | File Templates.
 */
trait HeuristicSolver extends HeuristicSolverT {

  private
  def probePoints(): List[Double] = List(-0.78, -0.25, 0, 0.25, 0.78)

  private
  def accuracy: Double = 1.0E-10

  private
  def abs_diff_values(v1: Option[Double], v2: Option[Double]): Option[Double] = {
    (v1, v2) match {
      case (Some(x), Some(y)) => Some(scala.math.abs(x - y))
      case _ => None
    }
  }


  private
  def continue_search(func: (Double) => Option[Double], approx: (Double) => Option[Double]): Boolean = {
    probePoints().map {
      case (x) => abs_diff_values(func(x), approx(x))
    }.reduce(liftedSum(_, _)).map(_ / probePoints().length) match {
      case Some(result) if (result > accuracy) => true
      case _ => false
    }
  }


  private
  def
  prune_helper(rev_coeff: List[Double]): List[Double] = if (rev_coeff.head < 0.01 * accuracy) prune_helper(rev_coeff.tail) else rev_coeff.reverse

  private
  def prune(coeff: List[Double]) = prune_helper(coeff.reverse)

  @tailrec
  private
  def heuristically_determine_approx(func: (Double) => Option[Double], app: approxT, interval: Interval, N: Int, iter: Int): Option[((Double) => Option[Double], Int)] = {
    val approx = app(func, N)
    //println("iter : ", iter, "  N : " + N)
    iter match {
      case 0 => None //doesn't converge so return NONE Some((approx, N))
      case _ if (continue_search(func, approx) == true) => heuristically_determine_approx(func, app, interval, 2 * N - 1, iter - 1)
      case _ => Some((approx, N))
    }
  }

  override
  protected
  def isEqualHeuristicSolver(other: Any) = other.isInstanceOf[HeuristicSolver]

  override
  protected
  val heuristicSolverString = heuristicSolverName(this)

  //-------------------------------------------------------
  override
  def heuristics(func: (Double) => Option[Double], app: approxT, interval: Interval, initialOrder: Int, maxIter: Int): Option[((Double) => Option[Double], Int)] = {
    heuristically_determine_approx(func, app, interval, initialOrder, maxIter)
  }

}
