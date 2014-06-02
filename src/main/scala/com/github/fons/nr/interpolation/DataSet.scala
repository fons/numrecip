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


  def apply(xvals: Vector[Double], yvals: Vector[Double]) = new DataSet(xvals, yvals)

  def apply(vals: Seq[(Double, Seq[Double])]) = {

    def collect(l: Vector[Vector[Double]], b: Vector[Double]) = {
      l.zip(b).map((lx) => lx._1 :+ lx._2)
    }

    if (vals.length > 0) {
      val xargs = vals.map(_._1).toVector
      val step1 = vals.map(_._2.toVector)
      val init = step1.head.map(Vector(_))
      val rem = step1.tail
      val yargs = rem.foldLeft(init)(collect)
      new DataSet((xargs, yargs))
    }
    else new DataSet(Vector[Double](), Vector[Double]())
  }
}

//TODO : persistence
//TODO : from/to file; csv,,,
case class DataSet(dataSet: (Vector[Double], Vector[Vector[Double]])) {
  def this(xvals: Vector[Double], yvals: Vector[Double]) = this((xvals, Vector(yvals)))

  val independend = dataSet._1
  val dependend = dataSet._2

  def apply(idx: Int): Option[Vector[Double]] = if (dataSet._2.isDefinedAt(idx) == true) Some(dataSet._2(idx)) else None

  def pp() {
    val number_of_data_points = dataSet._1.length
    val number_of_data_sets = dataSet._2.length
    val hd = (for (i <- Range(0, number_of_data_sets)) yield "dataset_" + (i + 1).toString + "  ").reduce(_ + _)
    val header = "  index  independent   " + hd
    println(header)
    for (index <- Range(0, number_of_data_points)) {
      print(f"$index%5d")
      print("  ")
      val val_1 = dataSet._1(index)
      print(f"$val_1%12.6f")
      print("  ")
      for (data_set_index <- Range(0, number_of_data_sets)) {
        val data_set = dataSet._2(data_set_index)
        val val_2 = data_set(index)
        print(f"$val_2%10.6f")
        print(" ")
      }
      println("")
    }
  }

  override
  def toString = "DataSet@" + hashCode() + " : [ number of values : " + dataSet._2 + "]"
}
