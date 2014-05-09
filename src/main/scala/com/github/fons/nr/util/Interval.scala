/*
 * Copyright (c) 2014.
 *
 * This file Interval.scala is part of numrecip (numrecip)
 *
 *     numrecip / Interval.scala is free software: you can redistribute it and/or modify
 *     it under the terms of the GNU General Public License as published by
 *     the Free Software Foundation, either version 3 of the License, or
 *     (at your option) any later version.
 *
 *     numrecip / Interval.scala is distributed in the hope that it will be useful,
 *     but WITHOUT ANY WARRANTY; without even the implied warranty of
 *     MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 *     GNU General Public License for more details.
 *
 *     You should have received a copy of the GNU General Public License
 *     along with numrecip.  If not, see <http://www.gnu.org/licenses/>.
 */

package com.github.fons.nr.util

/**
 * Created with IntelliJ IDEA.
 * User: fons
 * Date: 3/29/14
 * Time: 12:05 PM
 * To change this template use File | Settings | File Templates.
 */
case class Interval(from: Double, to: Double) {
  def inin(x: Double): Boolean = !((x < from) || (x > to))

  def inex(x: Double): Boolean = !(inin(x) || x == to)

  def exin(x: Double): Boolean = !(inin(x) || x == from)

  def exex(x: Double): Boolean = !(inin(x) || x == from || x == to)

  final
  override
  def equals(other: Any) = {
    val that = other.asInstanceOf[Interval]
    (that != null) && (from == that.from) && (that.to == to)
  }
}
