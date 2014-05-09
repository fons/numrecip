/*
 * Copyright (c) 2014.
 *
 * This file ApproximationT.scala is part of numrecip (numrecip)
 *
 *     numrecip / ApproximationT.scala is free software: you can redistribute it and/or modify
 *     it under the terms of the GNU General Public License as published by
 *     the Free Software Foundation, either version 3 of the License, or
 *     (at your option) any later version.
 *
 *     numrecip / ApproximationT.scala is distributed in the hope that it will be useful,
 *     but WITHOUT ANY WARRANTY; without even the implied warranty of
 *     MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 *     GNU General Public License for more details.
 *
 *     You should have received a copy of the GNU General Public License
 *     along with numrecip.  If not, see <http://www.gnu.org/licenses/>.
 */

package com.github.fons.nr.function.approximation

import com.github.fons.nr.util._
import com.github.fons.nr.util.Interval

/**
 * Created with IntelliJ IDEA.
 * User: fons
 * Date: 3/29/14
 * Time: 4:15 PM
 * To change this template use File | Settings | File Templates.
 */

trait ApproximationT {

  protected
  def approxName[A](a: A)(implicit m: Manifest[A]) = m.toString

  def apply(func: (Double) => Double, interval: Interval = Interval(-1, 1), order: Option[Int] = None): Option[FunctionApproximation] = {
    <>(toPartialFunction(interval, func), interval, order)
  }

  def <>(func: (Double) => Option[Double], interval: Interval = Interval(-1, 1), order: Option[Int] = None): Option[FunctionApproximation]

  def derivative(approximation: FunctionApproximation): Option[FunctionApproximation]

  def integral(x0: Double, approximation: FunctionApproximation): Option[FunctionApproximation]


}
