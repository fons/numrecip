/*
 * Copyright (c) 2014.
 *
 * This file BarycentricInterpolation.scala is part of numrecip (numrecip)
 *
 *     numrecip / BarycentricInterpolation.scala is free software: you can redistribute it and/or modify
 *     it under the terms of the GNU General Public License as published by
 *     the Free Software Foundation, either version 3 of the License, or
 *     (at your option) any later version.
 *
 *     numrecip / BarycentricInterpolation.scala is distributed in the hope that it will be useful,
 *     but WITHOUT ANY WARRANTY; without even the implied warranty of
 *     MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 *     GNU General Public License for more details.
 *
 *     You should have received a copy of the GNU General Public License
 *     along with numrecip.  If not, see <http://www.gnu.org/licenses/>.
 */

package com.github.fons.nr.function.approximation

/**
 * Created with IntelliJ IDEA.
 * User: fons
 * Date: 4/13/14
 * Time: 6:28 PM
 * To change this template use File | Settings | File Templates.
 */

import com.github.fons.nr.util._
import scala.Some

trait BarycentricInterpolation extends EvalStrategyT {


  private
  def highAccuracy = 1.0E-14

  private
  def atnode(x: Double): ((Double, Option[Double], Double)) => Option[Double] = {
    (v1: (Double, Option[Double], Double)) => {
      val (xj, fj, _) = v1
      //println("--> ", xj, x, x-xj)
      val diff = (x - xj)
      if (scala.math.abs(diff) < highAccuracy) fj else Some(0.0)
    }
  }

  private
  def reducer(lhs: Option[(Double, Double)], rhs: Option[(Double, Double)]) = {
    (lhs, rhs) match {
      case (Some((lhsn, lhsd)), Some((rhsn, rhsd))) => Some((lhsn + rhsn, lhsd + rhsd))
      case _ => None
    }
  }

  private
  def process(valx: Double) = {
    (v1: (Double, Option[Double], Double)) => {
      val (xj, fj, wj) = v1
      val diff = (valx - xj)
      val c = wj / diff
      //println("-> ", valx, diff, fj*c, c)
      fj.map((x: Double) => (x * c, c))
    }
  }

  private
  def zeroes(N: Int, func: (Double) => Option[Double]): List[(Double, Option[Double], Double)] = {
    (for (j <- Range(0, N + 1)) yield {
      val v = scala.math.cos(scala.math.Pi * j / (1.0 * N))
      val f = j match {
        case 0 => 0.5
        case N => 0.5
        case _ => 1.0
      }
      val w = if (2 * (j / 2) == j) f else -f
      (v, func(v), w)
    }).toList
  }

  override
  protected
  def isEqualEvalStrategy(other: Any) = other.isInstanceOf[BarycentricInterpolation]

  override
  protected
  val evalStrategyString = evalName(this)

  ///-------

  override
  def coefficients(N: Int, func: (Double) => Option[Double]): Option[List[Double]] =
    listToOption(zeroes(N, func).map {
      case (_, v, _) => v
    }, Some(List()))

  override
  def approximant(func: (Double) => Option[Double], order: Int): (Double) => Option[Double] = {
    val coeff: List[(Double, Option[Double], Double)] = zeroes(order, func)
    (x: Double) => {
      coeff.map(atnode(x)).reduce(liftedSum) match {
        case Some(result) if scala.math.abs(result) < highAccuracy => coeff.map(process(x)).reduce(reducer).map {
          case ((num, denom)) => num / denom
        }
        case Some(result) => Some(result)
        case _ => None
      }
    }
  }

}
