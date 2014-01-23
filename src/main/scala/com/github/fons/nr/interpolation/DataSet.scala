/*
 * Copyright (c) 2014.
 *
 * This file DataSet.scala is part of numrecip (numrecip)
 *
 *     numrecip / DataSet.scala is free software: you can redistribute it and/or modify
 *     it under the terms of the GNU General Public License as published by
 *     the Free Software Foundation, either version 3 of the License, or
 *     (at your option) any later version.
 *
 *     numrecip / DataSet.scala is distributed in the hope that it will be useful,
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
 * Time: 5:44 PM
 * To change this template use File | Settings | File Templates.
 */
object DataSet {
  def apply(xvals : Vector[Double], yvals : Vector[Double]) = new DataSet(xvals, yvals)
}
//TODO : persistence
//TODO : from/to file; cvs,,,
case class DataSet(dataSet : (Vector[Double], Vector[Vector[Double]])) {
  def this(xvals : Vector[Double], yvals : Vector[Double]) {
    this((xvals, Vector(yvals)))
  }

  def independend = dataSet._1
  val dependend   = dataSet._2
  def apply(idx:Int) : Option[Vector[Double]] = if (dataSet._2.isDefinedAt(idx) == true) Some(dataSet._2(idx)) else None

}
