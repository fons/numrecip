/*
 * Copyright (c) 2014.
 *
 * This file ChebShift.scala is part of numrecip (numrecip)
 *
 *     numrecip / ChebShift.scala is free software: you can redistribute it and/or modify
 *     it under the terms of the GNU General Public License as published by
 *     the Free Software Foundation, either version 3 of the License, or
 *     (at your option) any later version.
 *
 *     numrecip / ChebShift.scala is distributed in the hope that it will be useful,
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
 * Date: 3/29/14
 * Time: 7:50 PM
 * To change this template use File | Settings | File Templates.
 */

object ChebShift {
  def apply(from: Double, to: Double) = new ChebShift((x) => x, from, to)
}

case class ChebShift[R](func: (Double) => R, from: Double = -1.0, to: Double = 1.0) extends PartialFunction[Double, R] {
  private
  def g(t: Double) = (to - from) * 0.5 * t + 0.5 * (from + to)

  def apply() = func

  def apply(t: Double): R = func(g(t))

  def toDomain(x: Double) = -<-(x)

  def -<-(x: Double) = 2.0 * (x - from) / (to - from) - 1.0

  def ->-(t: Double) = g(t)

  def fromDomain(x: Double) = ->-(x)

  def isDefinedAt(x: Double): Boolean = !((x > 1.0) || (x < -1.0))

}
