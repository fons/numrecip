/*
 * Copyright (c) 2014.
 *
 * This file RectangularSpace.scala is part of numrecip (numrecip)
 *
 *     numrecip / RectangularSpace.scala is free software: you can redistribute it and/or modify
 *     it under the terms of the GNU General Public License as published by
 *     the Free Software Foundation, either version 3 of the License, or
 *     (at your option) any later version.
 *
 *     numrecip / RectangularSpace.scala is distributed in the hope that it will be useful,
 *     but WITHOUT ANY WARRANTY; without even the implied warranty of
 *     MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 *     GNU General Public License for more details.
 *
 *     You should have received a copy of the GNU General Public License
 *     along with numrecip.  If not, see <http://www.gnu.org/licenses/>.
 */

package com.github.fons.nr.util

/**
 * Created with IntelliJ IDEA.
 * User: fons
 * Date: 5/21/14
 * Time: 6:34 PM
 * To change this template use File | Settings | File Templates.
 */
object RectangularSpace {
  private
  val sampler = UniformSampler()

  def ppData(sep: String = " "): (Double, Double, Vector[Double]) => String = {
    def g(x: Double, y: Double, xs: Vector[Double]): String = {
      val xv = f"$x%10.6f"
      val yv = f"$y%10.6f"
      val xsv = xs.foldLeft("")((st: String, v1: Double) => st + sep + f" $v1%10.6f")
      xv + yv + xsv
    }
    g
  }
}

case class RectangularSpace(xinterval: Interval, xsteps: Int, yinterval: Interval, ysteps: Int, func: Vector[Function2[Double, Double, Double]]) {

  def this(xinterval: Interval, xsteps: Int, yinterval: Interval, ysteps: Int, func: Function2[Double, Double, Double]) = this(xinterval, xsteps, yinterval, ysteps, Vector(func))

  def this(xinterval: Interval, xsteps: Int, yinterval: Interval, ysteps: Int, func: Vector[Function2[Double, Double, Option[Double]]], default: Double = 0.0) = {
    this(xinterval, xsteps, yinterval, ysteps, func.map(toWholeFunction2(default)(_)))
  }

  //def this(interval:Interval, steps:Int, func: Function2[Double, Option[Double]], default:Double) = this(interval, steps, Vector(func), default)

  private
  def applyToAll(x: Double, y: Double): Vector[Double] = func.map((f: (Double, Double) => Double) => f(x, y))

  val data = {
    val b = (for (x <- RectangularSpace.sampler(xinterval, xsteps); y <- RectangularSpace.sampler(yinterval, ysteps)) yield {
      (x, y)
    })
    b.map {
      case (x, y) => x +: y +: applyToAll(x, y)
    }
  }

  def map[U](mfunc: (Double, Double, Vector[Double]) => U): Vector[U] = data.map {
    case (x +: y +: xs) => mfunc(x, y, xs)
  }

  def foreach(mfunc: (Double, Double, Vector[Double]) => Unit): Unit = data.foreach {
    case (x +: y +: xs) => mfunc(x, y, xs)
  }

  def pp() {
    val header = "%10s".format("index")
    //,  independent"
    print(header)
    for (i <- Range(0, data(0).length - 1)) yield {
      val s = ", " + "%10s".format("yval" + i)
      print(s)
    }
    println("")
    foreach((x, y, xs) => println(RectangularSpace.ppData(",")(x, y, xs))) //y.map(print..$y%10.6f"))
  }
}
