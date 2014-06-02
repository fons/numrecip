/*
 * Copyright (c) 2014.
 *
 * This file CubicSpline.scala is part of numrecip (numrecip)
 *
 *     numrecip / CubicSpline.scala is free software: you can redistribute it and/or modify
 *     it under the terms of the GNU General Public License as published by
 *     the Free Software Foundation, either version 3 of the License, or
 *     (at your option) any later version.
 *
 *     numrecip / CubicSpline.scala is distributed in the hope that it will be useful,
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
 * Date: 1/19/14
 * Time: 2:34 PM
 * To change this template use File | Settings | File Templates.
 */


trait CubicSpline extends InterpolatorT with SplineStrategy {

  override
  protected
  def strategyClassName[A](a: A)(implicit m: Manifest[A]) = m.toString

  private
  case class Spline(from: Double, to: Double, s1: Double, s2: Double, s3: Double, y: Double) extends InterpolationT {
    private def S(w: Double) = Some(((s3 * w + s2) * w + s1) * w + y)

    def apply(x: Double): Option[Double] = {
      if (x < from || x > to) None else S((x - from))
    }
  }

  private
  def initialize_helper(indep: Vector[Double], data: Vector[Double]): Option[Vector[InterpolationT]] = {

    lazy val delta_x = indep.zip(indep tail).map(xy => xy._2 - xy._1)
    lazy val delta_y = data.zip(data tail).map(xy => xy._2 - xy._1)
    lazy val rat_yx = delta_y.zip(delta_x).map((x) => x._1 / x._2)

    //returns the second order derivatives
    val Mm = strategy(indep: Vector[Double], data: Vector[Double])

    Mm match {
      case Some(m) => Some((for (index <- Range(0, indep.length - 1)) yield {
        val s1 = rat_yx(index) - delta_x(index) * (2.0 * m(index) + m(index + 1)) / 6.0
        val s2 = m(index) * .5
        val s3 = (m(index + 1) - m(index)) / (6.0 * delta_x(index))
        Spline(indep(index), indep(index + 1), s1, s2, s3, data(index))
      }).toVector)
      case _ => None
    }
  }

  override
  protected
  def initialize(dataSet: DataSet): Option[Vector[InterpolationSet]] = Some(for (y <- dataSet.dependend) yield InterpolationSet(initialize_helper(dataSet.independend, y)))

  override
  def interpolatorName = strategyClassName(this) + " with strategy " + strategyName

}
