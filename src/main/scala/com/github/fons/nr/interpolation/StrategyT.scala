/*
 * Copyright (c) 2014.
 *
 * This file StrategyT.scala is part of numrecip (numrecip)
 *
 *     numrecip / StrategyT.scala is free software: you can redistribute it and/or modify
 *     it under the terms of the GNU General Public License as published by
 *     the Free Software Foundation, either version 3 of the License, or
 *     (at your option) any later version.
 *
 *     numrecip / StrategyT.scala is distributed in the hope that it will be useful,
 *     but WITHOUT ANY WARRANTY; without even the implied warranty of
 *     MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 *     GNU General Public License for more details.
 *
 *     You should have received a copy of the GNU General Public License
 *     along with numrecip.  If not, see <http://www.gnu.org/licenses/>.
 */

package com.github.fons.nr.interpolation

/**
 * Created with IntelliJ IDEA.
 * User: fons
 * Date: 1/27/14
 * Time: 1:00 PM
 * To change this template use File | Settings | File Templates.
 */
trait StrategyT[T] {
  protected def strategyClassName[A](a: A)(implicit m: Manifest[A]) = m.toString
  def strategyName = "base strategy trait"

  def strategy(indep: Vector[Double], data: Vector[Double]) : Option[Vector[T]] = None


}
