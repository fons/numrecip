/*
 * Copyright (c) 2014.
 *
 * This file MemoizeT.scala is part of numrecip (numrecip)
 *
 *     numrecip / MemoizeT.scala is free software: you can redistribute it and/or modify
 *     it under the terms of the GNU General Public License as published by
 *     the Free Software Foundation, either version 3 of the License, or
 *     (at your option) any later version.
 *
 *     numrecip / MemoizeT.scala is distributed in the hope that it will be useful,
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
 * Date: 11/4/13
 * Time: 7:11 PM
 * To change this template use File | Settings | File Templates.
 */

trait MemoizeT {
  val list: Vector[(Double, List[Double])] = Vector[(Double, List[Double])]()

  def store(v: (Double, List[Double])) = {
    this
  }

  def toList = list.toList


  lazy
  val memoizeString = "com.mhsw.com.github.fons.nr.ode.MemoizeT@" + hashCode().toString + "@entries:" + (list length).toString
}
