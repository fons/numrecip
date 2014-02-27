/*
 * Copyright (c) 2014.
 *
 * This file DifferentiationExample1.scala is part of numrecip (numrecip)
 *
 *     numrecip / DifferentiationExample1.scala is free software: you can redistribute it and/or modify
 *     it under the terms of the GNU General Public License as published by
 *     the Free Software Foundation, either version 3 of the License, or
 *     (at your option) any later version.
 *
 *     numrecip / DifferentiationExample1.scala is distributed in the hope that it will be useful,
 *     but WITHOUT ANY WARRANTY; without even the implied warranty of
 *     MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 *     GNU General Public License for more details.
 *
 *     You should have received a copy of the GNU General Public License
 *     along with numrecip.  If not, see <http://www.gnu.org/licenses/>.
 */

package com.github.fons.nr.examples

import com.github.fons.nr.diff.DiffFunc


/**
 * Created with IntelliJ IDEA.
 * User: fons
 * Date: 2/25/14
 * Time: 6:53 PM
 * To change this template use File | Settings | File Templates.
 */
object DifferentiationExample1 {
  def run {
    def f(x:Double) = x*x + 4.0*x + scala.math.log(x)


    val k = DiffFunc(f, 0.1)
    val x = 10.0
    println("function : x*x + 4.0*x + scala.math.log(x)")
    println("derivative at " + x + k(x)())

    println("======================================")
  }
}
