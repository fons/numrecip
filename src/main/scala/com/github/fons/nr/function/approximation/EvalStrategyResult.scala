/*
 * Copyright (c) 2014.
 *
 * This file EvalStrategyResult.scala is part of numrecip (numrecip)
 *
 *     numrecip / EvalStrategyResult.scala is free software: you can redistribute it and/or modify
 *     it under the terms of the GNU General Public License as published by
 *     the Free Software Foundation, either version 3 of the License, or
 *     (at your option) any later version.
 *
 *     numrecip / EvalStrategyResult.scala is distributed in the hope that it will be useful,
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
 * Date: 5/7/14
 * Time: 6:36 PM
 * To change this template use File | Settings | File Templates.
 */
case class EvalStrategyResult(val approximant: (Double) => Option[Double], val coefficients: List[Double]) {

}
