/*
 * Copyright (c) 2014.
 *
 * This file DiffResult.scala is part of numrecip (numrecip)
 *
 *     numrecip / DiffResult.scala is free software: you can redistribute it and/or modify
 *     it under the terms of the GNU General Public License as published by
 *     the Free Software Foundation, either version 3 of the License, or
 *     (at your option) any later version.
 *
 *     numrecip / DiffResult.scala is distributed in the hope that it will be useful,
 *     but WITHOUT ANY WARRANTY; without even the implied warranty of
 *     MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 *     GNU General Public License for more details.
 *
 *     You should have received a copy of the GNU General Public License
 *     along with numrecip.  If not, see <http://www.gnu.org/licenses/>.
 */

package com.github.fons.nr.diff

/**
 * Created with IntelliJ IDEA.
 * User: fons
 * Date: 2/25/14
 * Time: 6:33 PM
 * To change this template use File | Settings | File Templates.
 */
case class DiffResult(index : Int, result : Map[(Int,Int),Double]) {

  def apply():Option[Double] = result.get((index,index))
  def error():Option[Double] = None
}
