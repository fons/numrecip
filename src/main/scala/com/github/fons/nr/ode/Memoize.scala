/*
 * Copyright (c) 2014.
 *
 * This file Memoize.scala is part of numrecip (numrecip)
 *
 *     numrecip / Memoize.scala is free software: you can redistribute it and/or modify
 *     it under the terms of the GNU General Public License as published by
 *     the Free Software Foundation, either version 3 of the License, or
 *     (at your option) any later version.
 *
 *     numrecip / Memoize.scala is distributed in the hope that it will be useful,
 *     but WITHOUT ANY WARRANTY; without even the implied warranty of
 *     MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 *     GNU General Public License for more details.
 *
 *     You should have received a copy of the GNU General Public License
 *     along with numrecip.  If not, see <http://www.gnu.org/licenses/>.
 */

package com.github.fons.nr.ode

/**
 * Created with IntelliJ IDEA.
 * User: fons
 * Date: 11/5/13
 * Time: 9:42 PM
 * To change this template use File | Settings | File Templates.
 */
trait Memoize extends MemoizeT {
  override def store(v: (Double, List[Double])) = {
    val L = list :+ v
    new Memoize {
      override val list = L
    }
  }

  override
  lazy
  val toString = "com.mhsw.com.github.fons.nr.ode.Memoize@" + hashCode().toString + "@entries:" + (list length).toString
}
