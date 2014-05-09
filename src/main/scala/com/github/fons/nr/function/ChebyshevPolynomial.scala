/*
 * Copyright (c) 2014.
 *
 * This file ChebyshevPolynomial.scala is part of numrecip (numrecip)
 *
 *     numrecip / ChebyshevPolynomial.scala is free software: you can redistribute it and/or modify
 *     it under the terms of the GNU General Public License as published by
 *     the Free Software Foundation, either version 3 of the License, or
 *     (at your option) any later version.
 *
 *     numrecip / ChebyshevPolynomial.scala is distributed in the hope that it will be useful,
 *     but WITHOUT ANY WARRANTY; without even the implied warranty of
 *     MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 *     GNU General Public License for more details.
 *
 *     You should have received a copy of the GNU General Public License
 *     along with numrecip.  If not, see <http://www.gnu.org/licenses/>.
 */

package com.github.fons.nr.function

/**
 * Created with IntelliJ IDEA.
 * User: fons
 * Date: 3/28/14
 * Time: 8:23 PM
 * To change this template use File | Settings | File Templates.
 */

case class ChebyshevPolynomial(N: Int) extends PartialFunction[Double, Double] {
  def apply(x: Double): Double = scala.math.cos(N * scala.math.acos(x))

  def isDefinedAt(x: Double): Boolean = !((x < -1.0) || (x > 1.0))
}
