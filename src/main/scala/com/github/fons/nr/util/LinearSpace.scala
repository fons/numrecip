/*
 * Copyright (c) 2014.
 *
 * This file LinearSpace.scala is part of numrecip (numrecip)
 *
 *     numrecip / LinearSpace.scala is free software: you can redistribute it and/or modify
 *     it under the terms of the GNU General Public License as published by
 *     the Free Software Foundation, either version 3 of the License, or
 *     (at your option) any later version.
 *
 *     numrecip / LinearSpace.scala is distributed in the hope that it will be useful,
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
 * Date: 5/12/14
 * Time: 6:28 PM
 * To change this template use File | Settings | File Templates.
 */

object LinearSpace {
  private
  val sampler = UniformSampler()

  def apply(interval: Interval, steps: Int, func: Function1[Double, Double]) = new LinearSpace(interval, steps, func)

  def apply(interval: Interval, steps: Int, func: Vector[Function1[Double, Option[Double]]], default: Double = 0) = new LinearSpace(interval, steps, func, default)

  def apply(interval: Interval, steps: Int, func: Function1[Double, Option[Double]], default: Double) = new LinearSpace(interval, steps, Vector(func), default)

  def ppData(sep: String = " "): (Double, Vector[Double]) => String = {
    def g(x: Double, y: Vector[Double]): String = {
      val xv = f"$x%10.6f"
      val yv = y.foldLeft("")((st: String, v1: Double) => st + sep + f" $v1%10.6f")
      xv + yv
    }
    g
  }

}

case class LinearSpace(val interval: Interval, val steps: Int, val func: Vector[Function1[Double, Double]]) {

  def this(interval: Interval, steps: Int, func: Function1[Double, Double]) = this(interval, steps, Vector(func))

  def this(interval: Interval, steps: Int, func: Vector[Function1[Double, Option[Double]]], default: Double = 0.0) = this(interval, steps, func.map(toWholeFunction(default)(_)))

  def this(interval: Interval, steps: Int, func: Function1[Double, Option[Double]], default: Double) = this(interval, steps, Vector(func), default)

  private
  def applyToAll(x: Double): Vector[Double] = func.map((f: (Double) => Double) => f(x))

  val data: Vector[Vector[Double]] = LinearSpace.sampler(interval, steps).map {
    case (x) => x +: applyToAll(x)
  }

  def map[U](mfunc: (Double, Vector[Double]) => U): Vector[U] = data.map {
    case (x +: xs) => mfunc(x, xs)
  }

  def foreach(mfunc: (Double, Vector[Double]) => Unit): Unit = data.foreach {
    case (x +: xs) => mfunc(x, xs)
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
    foreach((x, y) => println(LinearSpace.ppData(",")(x, y))) //y.map(print..$y%10.6f"))
  }

}
