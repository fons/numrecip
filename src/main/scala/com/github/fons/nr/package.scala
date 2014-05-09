/*
 * Copyright (c) 2014.
 *
 * This file package.scala is part of numrecip (numrecip)
 *
 *     numrecip / package.scala is free software: you can redistribute it and/or modify
 *     it under the terms of the GNU General Public License as published by
 *     the Free Software Foundation, either version 3 of the License, or
 *     (at your option) any later version.
 *
 *     numrecip / package.scala is distributed in the hope that it will be useful,
 *     but WITHOUT ANY WARRANTY; without even the implied warranty of
 *     MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 *     GNU General Public License for more details.
 *
 *     You should have received a copy of the GNU General Public License
 *     along with numrecip.  If not, see <http://www.gnu.org/licenses/>.
 */

package com.github.fons.nr

import scala.annotation.tailrec

/**
 * Created with IntelliJ IDEA.
 * User: fons
 * Date: 1/31/14
 * Time: 3:37 PM
 * To change this template use File | Settings | File Templates.
 */
package object util {


  def toPartialFunction(interval: Interval, func: (Double) => Double): (Double) => Option[Double] = {
    (x: Double) => {
      if (interval.inin(x)) Some(func(x)) else None
    }
  }


  def liftedSum(x: Option[Double], y: Option[Double]): Option[Double] = LiftedOperation((v1: Double, v2: Double) => v1 + v2)(x, y)

  def liftedSub(x: Option[Double], y: Option[Double]): Option[Double] = LiftedOperation((v1: Double, v2: Double) => v1 - v2)(x, y)

  def liftedProd(x: Option[Double], y: Option[Double]): Option[Double] = LiftedOperation((v1: Double, v2: Double) => v1 * v2)(x, y)

  def liftedDiv(x: Option[Double], y: Option[Double]): Option[Double] = LiftedOperation((v1: Double, v2: Double) => v1 / v2)(x, y)

  @tailrec
  def listToOption(l: List[Option[Double]], accum: Option[List[Double]] = Some(List())): Option[List[Double]] = {
    (l, accum) match {
      case (_, None) => None
      case (List(), _) => accum.map(_.reverse)
      case (Some(v1) :: vs, Some(acc)) => listToOption(vs, Some(v1 :: acc))
      case (None :: vs, _) => None
    }
  }

  def overflowthrow(v: Double) = {
    v match {
      case Double.PositiveInfinity => throw new ArithmeticException("positive overflow")
      case Double.NegativeInfinity => throw new ArithmeticException("negative overflow")
      case Double.NaN => throw new ArithmeticException("not an number")
      case _ => v
    }
  }

  def overflowOption(v: Double): Option[Double] = {
    v match {
      case Double.PositiveInfinity => None
      case Double.NegativeInfinity => None
      case Double.NaN => None
      case _ => Some(v)
    }
  }
}
