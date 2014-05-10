/*
 * Copyright (c) 2014.
 *
 * This file EvalStrategyT.scala is part of numrecip (numrecip)
 *
 *     numrecip / EvalStrategyT.scala is free software: you can redistribute it and/or modify
 *     it under the terms of the GNU General Public License as published by
 *     the Free Software Foundation, either version 3 of the License, or
 *     (at your option) any later version.
 *
 *     numrecip / EvalStrategyT.scala is distributed in the hope that it will be useful,
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
 * Time: 5:50 PM
 * To change this template use File | Settings | File Templates.
 */

trait EvalStrategyT {

  protected
  def isEqualEvalStrategy(other: Any): Boolean = false

  protected
  def evalName[A](a: A)(implicit m: Manifest[A]) = m.toString

  protected
  val evalStrategyString = evalName(this)

  def coefficients(N: Int, func: (Double) => Option[Double]): Option[List[Double]] = None

  def approximant(func: (Double) => Option[Double], order: Int): (Double) => Option[Double] = (x: Double) => None

  def derivative(coeff: List[Double]): Option[EvalStrategyResult] = None

  def integral(x0: Double, coeff: List[Double]): Option[EvalStrategyResult] = None

}