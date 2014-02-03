/*
 * Copyright (c) 2014.
 *
 * This file LagrangeNeville.scala is part of numrecip (numrecip)
 *
 *     numrecip / LagrangeNeville.scala is free software: you can redistribute it and/or modify
 *     it under the terms of the GNU General Public License as published by
 *     the Free Software Foundation, either version 3 of the License, or
 *     (at your option) any later version.
 *
 *     numrecip / LagrangeNeville.scala is distributed in the hope that it will be useful,
 *     but WITHOUT ANY WARRANTY; without even the implied warranty of
 *     MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 *     GNU General Public License for more details.
 *
 *     You should have received a copy of the GNU General Public License
 *     along with numrecip.  If not, see <http://www.gnu.org/licenses/>.
 */

package com.github.fons.nr.interpolation

import scala.annotation.tailrec

/**
 * Created with IntelliJ IDEA.
 * User: fons
 * Date: 1/25/14
 * Time: 6:36 PM
 * To change this template use File | Settings | File Templates.
 */
trait PolynomialApproximation extends InterpolatorT with StrategyT[InterpolationT] with Degree {

  private
  def initialize_helper(indep: Vector[Double], data: Vector[Double]): Option[Vector[InterpolationT]] = strategy(indep, data)

  override
  protected
  def initialize(dataSet: DataSet): Option[Vector[InterpolationSet]] = Some(for (y <- dataSet.dependend) yield InterpolationSet(initialize_helper(dataSet.independend, y)))

  override
  def interpolatorName: String = strategyClassName(this) + " using strategy : " + strategyName + " with Degree " + degree
}
