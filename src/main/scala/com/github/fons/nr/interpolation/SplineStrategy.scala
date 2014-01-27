/*
 * Copyright (c) 2014.
 *
 * This file SplineStrategyT.scala is part of numrecip (numrecip)
 *
 *     numrecip / SplineStrategyT.scala is free software: you can redistribute it and/or modify
 *     it under the terms of the GNU General Public License as published by
 *     the Free Software Foundation, either version 3 of the License, or
 *     (at your option) any later version.
 *
 *     numrecip / SplineStrategyT.scala is distributed in the hope that it will be useful,
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
 * Date: 1/20/14
 * Time: 6:35 PM
 * To change this template use File | Settings | File Templates.
 */



trait SplineStrategy  extends StrategyT[Double] {

  //def strategy(indep: Vector[Double], data: Vector[Double]) : Option[Vector[Double]] = None

  override
  def strategyName = strategyClassName(this)
}
