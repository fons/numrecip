/*
 * Copyright (c) 2014.
 *
 * This file Beta.scala is part of numrecip (numrecip)
 *
 *     numrecip / Beta.scala is free software: you can redistribute it and/or modify
 *     it under the terms of the GNU General Public License as published by
 *     the Free Software Foundation, either version 3 of the License, or
 *     (at your option) any later version.
 *
 *     numrecip / Beta.scala is distributed in the hope that it will be useful,
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
 * Date: 5/20/14
 * Time: 7:00 PM
 * To change this template use File | Settings | File Templates.
 */
object Beta {
  def apply(z: Double, w: Double): Double = (Gamma(z) * Gamma(w)) / (Gamma(z + w))

  def apply(z: Double): (Double) => Double = {
    (w: Double) => this.apply(z, w)
  }

  def ln(z: Double, w: Double): Double = Gamma.ln(z) + Gamma.ln(w) - Gamma.ln(z + w)
}
