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

/**
 * Created with IntelliJ IDEA.
 * User: fons
 * Date: 1/31/14
 * Time: 3:37 PM
 * To change this template use File | Settings | File Templates.
 */
package object util {


  def overflowthrow(v: Double) = {
    v match {
      case Double.PositiveInfinity => throw new ArithmeticException("positive overflow")
      case Double.NegativeInfinity => throw new ArithmeticException("negative overflow")
      case Double.NaN => throw new ArithmeticException("not an number")
      case _ => v
    }
  }

  def overflowOption(v:Double) : Option[Double] = {
    v match {
      case Double.PositiveInfinity => None
      case Double.NegativeInfinity => None
      case Double.NaN => None
      case _ => Some(v)
    }
  }
}
